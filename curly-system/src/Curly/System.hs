{-# LANGUAGE UndecidableInstances, RecursiveDo, ScopedTypeVariables #-}
module Curly.System (
  -- * All known systems
  knownSystems,hostSystem,
  -- * Specializing for imperative systems
  specialize,specializeStandalone,
  -- * Just-in-time compiling
  JITContext,newJITContext,jitExpr
  ) where

import Definitive 
import Curly.Core
import Curly.Core.Annotated
import Curly.Core.Library
import Curly.Core.Documentation
import Curly.System.Base
import qualified Curly.System.X86.Linux as X86_Linux
import qualified Curly.System.ARM.Linux as ARM_Linux
import qualified Curly.System.JavaScript as JavaScript
import qualified Curly.System.HTML as HTML
import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.StablePtr
import Foreign.C.String (castCCharToChar)
import Foreign.Storable (peek,poke)

knownSystems :: Map String System
knownSystems = fromAList [(_sysName s,s) | s <- [hostSystem
                                                ,X86_Linux.system,X86_Linux.system64
                                                ,ARM_Linux.system
                                                ,JavaScript.system,JavaScript.systemASM
                                                ,HTML.system]]
hostSystem :: System
hostSystem = X86_Linux.system64 { _sysName = "host" }

mkRunExpr e = mkApply e (mkSymbol (Builtin zero B_Unit))

setDest :: (?sys :: VonNeumannMachine,IsValue v,IsValue v',MonadASM m s) => v -> v' -> m ()
setDest t v = do
  destReg!TypeOffset  <-- t
  destReg!ValueOffset <-- v

specialize :: forall m s. (?sys :: VonNeumannMachine,MonadASM m s,Show s,Documented s,Identifier s) => AnnExpr s -> m BinAddress
specialize expr = inSection TextSection $ getCounter <* specTail (sem expr)
  where
    specLambda e = get >>= \m -> mute $ case m^.rtAddresses.at e of
      Just a -> return a
      Nothing -> mfix $ \r -> do
        rtAddresses =~ insert e r
        inSection TextSection $ getCounter <* do
          builtinArgs 1
          specTail (sem e)

    specTail e = do
      specHead e
      tailCall destReg

    specHead (SemSymbol (Argument n)) = do
      f <- global_argFun
      setDest f (composing (const (!EnvOffset)) [1..n] (thisReg!ValueOffset))
    specHead (SemSymbol (Builtin _ b)) = case _curlyBuiltin ?sys b of
      Just mav -> uncurry setDest =<< mav
      Nothing -> error $ format "The builtin %s is not yet implemented on this system." (show b)
    specHead (SemAbstract _ body) = do
      a <- specLambda body
      setDest a (if empty (exprRefs body) then toValue (0 :: Int) else toValue (thisReg!ValueOffset))
    specHead (SemApply f x) = specAps f [x]
      where specAps (PatApply f' x) l = specAps f' (x:l)
            specAps f l = do
              pushing [destReg] $ do
                destReg <-- (0 :: Int)
                for_ (reverse (f:l)) $ \arg -> do
                  pushThunk destReg
                  specHead (sem arg)
                tmpReg <-- destReg
              szth <- global_partialApply (length l)
              setDest szth tmpReg


specializeStandalone :: System -> LeafExpr GlobalID -> Bytes
specializeStandalone sys e = let ?sys = sys in
  let Id (_,_,bin) = runASMT defaultRuntime $ do
        standalone (_sysStandalone sys) $ case _sysImpl sys of
          Imperative imp -> let ?sys = imp (_sysStandaloneHooks sys)
                            in specialize (mkRunExpr $ anonymous (e^.leafVal))
          RawSystem r -> inSection TextSection (getCounter <* tell (bytesCode' (r e)))
  in bin^.bData

data JITData s = JITData {
  _jd_runtime :: Runtime s,
  _jd_sections :: Map Section [ForeignPtr ()]
  }
jd_runtime :: Lens (Runtime s) (Runtime s') (JITData s) (JITData s')
jd_runtime = lens _jd_runtime (\x y -> x { _jd_runtime = y })
jd_sections :: Lens' (JITData s) (Map Section [ForeignPtr ()])
jd_sections = lens _jd_sections (\x y -> x { _jd_sections = y })
data JITContext s = JITContext (IORef (JITData s))

type RunJITExpr = IO ()
runJIT :: JITContext s -> ASMT s Id BinAddress -> IO RunJITExpr
runJIT (JITContext cxt) asm = let allocSections = [InitSection,TextSection,DataSection] in mdo
  rt <- runAtomic cxt $ do
    let a *+ b = (a*b) + (a+b)
    jd_sections =~ \x -> map pure fptrs *+ x
    let withJITRuntime m = let ?sys = jit_machine in do
          rtSections =~ (# [(sec,(zero,BA $ mlookup sec start)) | sec <- allocSections])
          (dest,this) <- inSection DataSection $ do
            align thunkSize 0
            liftA2 (,) (getCounter <* reserve thunkSize 0) (getCounter <* reserve thunkSize 0)
          start <- inSection TextSection m
          inSection InitSection $ do
            pushing [destReg,thisReg,tmpReg,poolReg] $ do
              destReg <-- dest
              thisReg <-- this
              poolReg <-- (0 :: Int)
              call start
            ret
    jd_runtime <~ \rt -> let Id ~(_,rt',_) = runASMT rt (withJITRuntime asm)
                         in (rt',rt')
  fptrs <- map (c'map . fromAList) $ for allocSections $ \sec -> do
    let (bc,_) = rt^.rtSection sec
    fptr <- mallocForeignPtrBytes (bc^.bcEstimate)
    logLine Debug $ format "Allocated JIT buffer of size %d at %s" (bc^.bcEstimate) (show fptr)
    return (sec,fptr)
  start <- for fptrs $ \fptr -> do
    withForeignPtr fptr $ \p -> return (fromIntegral (ptrToIntPtr p))
  for_ (fptrs^.ascList) $ \(sec,fptr) -> do
    let (bc,_) = rt^.rtSection sec
    withForeignPtr fptr $ \p -> do
      pokeArray (castPtr p) (unpack (bc^.bData))
      let pageStart = alignPtr (p`plusPtr`(1-jit_pageSize)) jit_pageSize
          protLength = fromIntegral $ bc^.bcEstimate + p`minusPtr`pageStart
      logLine Debug $ format "Marking JIT buffer (%s,+%s) as executable" (show pageStart) (show protLength)
      mprotect pageStart protLength (pROT_READ + pROT_WRITE + pROT_EXEC)
  let runIt = do
        let fp = castPtrToFunPtr $ intPtrToPtr $ fromIntegral $ mlookup InitSection start
        runIOFunPtr fp
  return runIt

type Wrapper t = t -> IO (FunPtr t)

class CCallable f where
  wrapper :: Wrapper f
                                               
foreign import ccall "dynamic" runIOFunPtr :: FunPtr (IO ()) -> IO ()

hsAddr :: CCallable a => a -> BinAddress
hsAddr fun = BA (fromIntegral (ptrToIntPtr (castFunPtrToPtr p)))
  where p = wrapper fun^.thunk
mallocAddr :: BinAddress
mallocAddr = hsAddr mallocBytes

type JIT_Expr = Ptr ()
jit_mkExprSymbol :: Int -> Ptr CChar -> IO JIT_Expr
jit_mkExprSymbol n p = do
  str <- map castCCharToChar <$> peekArray n p
  sp <- newStablePtr (mkSymbol str :: Expression String String)
  return (castStablePtrToPtr sp)
jit_mkExprLambda :: Int -> Ptr CChar -> JIT_Expr -> IO JIT_Expr
jit_mkExprLambda n ps pe = do
  str <- map castCCharToChar <$> peekArray n ps
  e <- deRefStablePtr (castPtrToStablePtr pe)
  putStrLn $ format "mkExprLambda %s %s" str (show e)
  sp <- newStablePtr (mkAbstract str e :: Expression String String)
  return (castStablePtrToPtr sp)
jit_mkExprApply :: JIT_Expr -> JIT_Expr -> IO JIT_Expr
jit_mkExprApply pf px  = do
  f <- deRefStablePtr (castPtrToStablePtr pf)
  x <- deRefStablePtr (castPtrToStablePtr px)
  putStrLn $ format "mkExprApply %s %s" (show f) (show x)
  sp <- newStablePtr (mkApply f x :: Expression String String)
  return (castStablePtrToPtr sp)
jit_exprInd :: JIT_Expr -> IO (Ptr ())
jit_exprInd pe = let ?sys = jit_machine in do
  e <- deRefStablePtr (castPtrToStablePtr pe)
  case e :: Expression String String of
    Join (Apply f x) -> do
      pf <- castStablePtrToPtr <$> newStablePtr f
      px <- castStablePtrToPtr <$> newStablePtr x
      mallocBytes (3*wordSize) <*= \pret -> do
        poke (castPtr pret) (0 :: Int)
        poke (castPtr (pret`plusPtr`wordSize)) pf
        poke (castPtr (pret`plusPtr`(2*wordSize))) px
    Join (Lambda s e) -> do
      pe <- castStablePtrToPtr <$> newStablePtr e
      ps <- mallocBytes (2*wordSize + length s)
      do poke (castPtr ps) (1 :: Int)
         poke (castPtr (ps`plusPtr`wordSize)) (length s)
         pokeArray (castPtr (ps`plusPtr`(2*wordSize))) s
      mallocBytes (3*wordSize) <*= \pret -> do
        poke (castPtr pret) (1 :: Int)
        poke (castPtr (pret`plusPtr`wordSize)) ps
        poke (castPtr (pret`plusPtr`(2*wordSize))) pe
    Pure s -> do
      ps <- mallocBytes (2*wordSize + length s)
      do poke (castPtr ps) (1 :: Int)
         poke (castPtr (ps`plusPtr`wordSize)) (length s)
         pokeArray (castPtr (ps`plusPtr`(2*wordSize))) s
      mallocBytes (2*wordSize) <*= \pret -> do
        poke (castPtr pret) (2 :: Int)
        poke (castPtr (pret`plusPtr`wordSize)) ps

jit_memextend_pool sz = getOrDefine TextSection ("memextend-pool-"+show sz) $ do
  ccall (Just poolReg) mallocAddr [return (Constant pageSize)]
  pushing [poolReg] $ do
    tmpReg <-- poolReg
    add tmpReg (pageSize :: Int)
    begin <- newFunction TextSection
    ifcmp (True,LT) poolReg tmpReg $ do
      poolReg!Offset 0 <-- poolReg
      add (poolReg!Offset 0) (sz :: Int)
      add poolReg (sz :: Int)
      jmp begin
  ret

ignore :: MonadASM m s => m () -> m ()
ignore m = m
jit_allocBytes l v = ignore $ let ?sys = jit_machine in ccall (Just l) mallocAddr [return v]

jit_pushThunk dest = ignore $ let ?sys = jit_machine in do
  ifcmp (True,EQ) poolReg (0 :: Integer) $ do
    call =<< jit_memextend_pool thunkSize
  poolReg ! EnvOffset <-- V dest
  dest <-- poolReg
  poolReg <-- poolReg ! Offset 0
jit_popThunk dest = ignore $ let ?sys = jit_machine in do
  dest ! Offset 0 <-- poolReg
  poolReg <-- dest
  dest <-- dest ! EnvOffset

jit_builtin B_ExprSym = Just $ getOrDefineBuiltin0 TextSection "mkExprSymbol" $ do
  [str] <- builtinArgs 1
  pushing [thisReg] $ callThunk str
  ccall2 (Just (thisReg!ValueOffset)) (hsAddr jit_mkExprSymbol) 
    (pure (str!ValueOffset!Offset wordSize))
    (do tmpReg <-- str!ValueOffset
        add tmpReg (2*wordSize :: Integer)
        return tmpReg)
  cst <- global_constant
  thisReg!TypeOffset <-- cst
  jmp cst
jit_builtin B_ExprLambda = Just $ getOrDefineBuiltin0 TextSection "mkExprLambda" $ do
  [param,body] <- builtinArgs 2
  pushing [thisReg] $ callThunk param
  pushing [thisReg] $ callThunk body
  
  ccall3 (Just (thisReg!ValueOffset)) (hsAddr jit_mkExprLambda) 
    (pure (param!ValueOffset!Offset wordSize))
    (do tmpReg <-- param!ValueOffset
        add tmpReg (2*wordSize :: Integer)
        return tmpReg)
    (pure (body!ValueOffset))
  cst <- global_constant
  thisReg!TypeOffset <-- cst
  jmp cst
jit_builtin B_ExprApply = Just $ getOrDefineBuiltin0 TextSection "mkExprApply" $ do
  [f,x] <- builtinArgs 2
  pushing [thisReg] $ callThunk f
  pushing [thisReg] $ callThunk x
  ccall2 (Just (thisReg!ValueOffset)) (hsAddr jit_mkExprApply) (pure (f!ValueOffset)) (pure (x!ValueOffset))
  cst <- global_constant
  thisReg!TypeOffset <-- cst
  jmp cst
jit_builtin B_ExprInd = Just $ getOrDefineBuiltin0 TextSection "exprInd" $ mdo
  [e,kl,ka,ks] <- builtinArgs 4
  pushing [thisReg] $ callThunk e
  ccall1 (Just tmpReg) (hsAddr jit_exprInd) (pure (e!ValueOffset))
  vTable <- getOrDefine DataSection "exprInd_t1" $ do
    for_ [onAp,onLam,onSym] (\(BA n) -> tell (binaryCode (Just wordSize,wordSize) n))
  add (tmpReg!Offset 0) vTable
  jmp (tmpReg!Offset 0)
  
  cst <- global_constant
  let on2 k = do
        pushing [tmpReg] $ callThunk k
        pushThunk (destReg!ValueOffset)
        destReg!ValueOffset!TypeOffset <-- cst
        destReg!ValueOffset!ValueOffset <-- tmpReg!Offset wordSize
        pushing [tmpReg] $ callThunk destReg
        pushThunk (destReg!ValueOffset)
        destReg!ValueOffset!TypeOffset <-- cst
        destReg!ValueOffset!ValueOffset <-- tmpReg!Offset (2*wordSize)
        tailCall destReg
      
  onLam <- getCounter <* on2 kl
  onAp <- getCounter <* on2 ka
  onSym <- getCounter <* do
    pushing [thisReg,tmpReg] $ callThunk ks
    pushThunk (destReg!ValueOffset)
    destReg!ValueOffset!TypeOffset <-- cst
    destReg!ValueOffset!ValueOffset <-- tmpReg!Offset wordSize
    tailCall destReg
  return ()
  
jit_builtin _ = Nothing

jit_machine :: VonNeumannMachine
jit_machine = let Imperative imp = _sysImpl hostSystem
                  sys = let ?sys = sys
                        in withAdditionalBuiltins jit_builtin $
                           imp $ Just $ SystemHooks jit_pushThunk jit_popThunk jit_allocBytes
              in sys
newJITContext :: IO (JITContext s)
newJITContext = map JITContext (newIORef (JITData defaultRuntime zero))
jitExpr :: (Documented s,Identifier s) => JITContext s -> AnnExpr s -> IO RunJITExpr
jitExpr cxt e = let ?sys = jit_machine in runJIT cxt (specialize (mkRunExpr e))

foreign import ccall "mprotect"
  mprotect :: Ptr a -> CSize -> CInt -> IO ()
foreign import ccall "getpagesize"
  getpagesize :: IO CInt
jit_pageSize :: Int
jit_pageSize = fromIntegral (getpagesize^.thunk)
instance Semigroup CInt
pROT_READ, pROT_WRITE, pROT_EXEC :: CInt
pROT_READ = 1
pROT_WRITE = 2
pROT_EXEC = 4

foreign import ccall "wrapper" _wrapper__  :: Wrapper (IO ())
instance CCallable (IO ()) where wrapper = _wrapper__
foreign import ccall "wrapper" _wrapper__i :: Wrapper (IO Int)
instance CCallable (IO Int) where wrapper = _wrapper__i
foreign import ccall "wrapper" _wrapper__p :: Wrapper (IO (Ptr a))
instance CCallable (IO (Ptr a)) where wrapper = _wrapper__p
foreign import ccall "wrapper" _wrapper_i_  :: Wrapper (Int -> IO ())
instance CCallable (Int -> IO ()) where wrapper = _wrapper_i_
foreign import ccall "wrapper" _wrapper_i_i :: Wrapper (Int -> IO Int)
instance CCallable (Int -> IO Int) where wrapper = _wrapper_i_i
foreign import ccall "wrapper" _wrapper_i_p :: Wrapper (Int -> IO (Ptr a))
instance CCallable (Int -> IO (Ptr a)) where wrapper = _wrapper_i_p
foreign import ccall "wrapper" _wrapper_p_  :: Wrapper (Ptr b -> IO ())
instance CCallable (Ptr b -> IO ()) where wrapper = _wrapper_p_
foreign import ccall "wrapper" _wrapper_p_i :: Wrapper (Ptr b -> IO Int)
instance CCallable (Ptr b -> IO Int) where wrapper = _wrapper_p_i
foreign import ccall "wrapper" _wrapper_p_p :: Wrapper (Ptr b -> IO (Ptr a))
instance CCallable (Ptr b -> IO (Ptr a)) where wrapper = _wrapper_p_p
foreign import ccall "wrapper" _wrapper_ii_  :: Wrapper (Int -> Int -> IO ())
instance CCallable (Int -> Int -> IO ()) where wrapper = _wrapper_ii_
foreign import ccall "wrapper" _wrapper_ii_i :: Wrapper (Int -> Int -> IO Int)
instance CCallable (Int -> Int -> IO Int) where wrapper = _wrapper_ii_i
foreign import ccall "wrapper" _wrapper_ii_p :: Wrapper (Int -> Int -> IO (Ptr a))
instance CCallable (Int -> Int -> IO (Ptr a)) where wrapper = _wrapper_ii_p
foreign import ccall "wrapper" _wrapper_ip_  :: Wrapper (Int -> Ptr b -> IO ())
instance CCallable (Int -> Ptr b -> IO ()) where wrapper = _wrapper_ip_
foreign import ccall "wrapper" _wrapper_ip_i :: Wrapper (Int -> Ptr b -> IO Int)
instance CCallable (Int -> Ptr b -> IO Int) where wrapper = _wrapper_ip_i
foreign import ccall "wrapper" _wrapper_ip_p :: Wrapper (Int -> Ptr b -> IO (Ptr a))
instance CCallable (Int -> Ptr b -> IO (Ptr a)) where wrapper = _wrapper_ip_p
foreign import ccall "wrapper" _wrapper_pi_  :: Wrapper (Ptr c -> Int -> IO ())
instance CCallable (Ptr c -> Int -> IO ()) where wrapper = _wrapper_pi_
foreign import ccall "wrapper" _wrapper_pi_i :: Wrapper (Ptr c -> Int -> IO Int)
instance CCallable (Ptr c -> Int -> IO Int) where wrapper = _wrapper_pi_i
foreign import ccall "wrapper" _wrapper_pi_p :: Wrapper (Ptr c -> Int -> IO (Ptr a))
instance CCallable (Ptr c -> Int -> IO (Ptr a)) where wrapper = _wrapper_pi_p
foreign import ccall "wrapper" _wrapper_pp_  :: Wrapper (Ptr c -> Ptr b -> IO ())
instance CCallable (Ptr c -> Ptr b -> IO ()) where wrapper = _wrapper_pp_
foreign import ccall "wrapper" _wrapper_pp_i :: Wrapper (Ptr c -> Ptr b -> IO Int)
instance CCallable (Ptr c -> Ptr b -> IO Int) where wrapper = _wrapper_pp_i
foreign import ccall "wrapper" _wrapper_pp_p :: Wrapper (Ptr c -> Ptr b -> IO (Ptr a))
instance CCallable (Ptr c -> Ptr b -> IO (Ptr a)) where wrapper = _wrapper_pp_p
foreign import ccall "wrapper" _wrapper_iii_  :: Wrapper (Int -> Int -> Int -> IO ())
instance CCallable (Int -> Int -> Int -> IO ()) where wrapper = _wrapper_iii_
foreign import ccall "wrapper" _wrapper_iii_i :: Wrapper (Int -> Int -> Int -> IO Int)
instance CCallable (Int -> Int -> Int -> IO Int) where wrapper = _wrapper_iii_i
foreign import ccall "wrapper" _wrapper_iii_p :: Wrapper (Int -> Int -> Int -> IO (Ptr a))
instance CCallable (Int -> Int -> Int -> IO (Ptr a)) where wrapper = _wrapper_iii_p
foreign import ccall "wrapper" _wrapper_iip_  :: Wrapper (Int -> Int -> Ptr b -> IO ())
instance CCallable (Int -> Int -> Ptr b -> IO ()) where wrapper = _wrapper_iip_
foreign import ccall "wrapper" _wrapper_iip_i :: Wrapper (Int -> Int -> Ptr b -> IO Int)
instance CCallable (Int -> Int -> Ptr b -> IO Int) where wrapper = _wrapper_iip_i
foreign import ccall "wrapper" _wrapper_iip_p :: Wrapper (Int -> Int -> Ptr b -> IO (Ptr a))
instance CCallable (Int -> Int -> Ptr b -> IO (Ptr a)) where wrapper = _wrapper_iip_p
foreign import ccall "wrapper" _wrapper_ipi_  :: Wrapper (Int -> Ptr c -> Int -> IO ())
instance CCallable (Int -> Ptr c -> Int -> IO ()) where wrapper = _wrapper_ipi_
foreign import ccall "wrapper" _wrapper_ipi_i :: Wrapper (Int -> Ptr c -> Int -> IO Int)
instance CCallable (Int -> Ptr c -> Int -> IO Int) where wrapper = _wrapper_ipi_i
foreign import ccall "wrapper" _wrapper_ipi_p :: Wrapper (Int -> Ptr c -> Int -> IO (Ptr a))
instance CCallable (Int -> Ptr c -> Int -> IO (Ptr a)) where wrapper = _wrapper_ipi_p
foreign import ccall "wrapper" _wrapper_ipp_  :: Wrapper (Int -> Ptr c -> Ptr b -> IO ())
instance CCallable (Int -> Ptr c -> Ptr b -> IO ()) where wrapper = _wrapper_ipp_
foreign import ccall "wrapper" _wrapper_ipp_i :: Wrapper (Int -> Ptr c -> Ptr b -> IO Int)
instance CCallable (Int -> Ptr c -> Ptr b -> IO Int) where wrapper = _wrapper_ipp_i
foreign import ccall "wrapper" _wrapper_ipp_p :: Wrapper (Int -> Ptr c -> Ptr b -> IO (Ptr a))
instance CCallable (Int -> Ptr c -> Ptr b -> IO (Ptr a)) where wrapper = _wrapper_ipp_p
foreign import ccall "wrapper" _wrapper_pii_  :: Wrapper (Ptr d -> Int -> Int -> IO ())
instance CCallable (Ptr d -> Int -> Int -> IO ()) where wrapper = _wrapper_pii_
foreign import ccall "wrapper" _wrapper_pii_i :: Wrapper (Ptr d -> Int -> Int -> IO Int)
instance CCallable (Ptr d -> Int -> Int -> IO Int) where wrapper = _wrapper_pii_i
foreign import ccall "wrapper" _wrapper_pii_p :: Wrapper (Ptr d -> Int -> Int -> IO (Ptr a))
instance CCallable (Ptr d -> Int -> Int -> IO (Ptr a)) where wrapper = _wrapper_pii_p
foreign import ccall "wrapper" _wrapper_pip_  :: Wrapper (Ptr d -> Int -> Ptr b -> IO ())
instance CCallable (Ptr d -> Int -> Ptr b -> IO ()) where wrapper = _wrapper_pip_
foreign import ccall "wrapper" _wrapper_pip_i :: Wrapper (Ptr d -> Int -> Ptr b -> IO Int)
instance CCallable (Ptr d -> Int -> Ptr b -> IO Int) where wrapper = _wrapper_pip_i
foreign import ccall "wrapper" _wrapper_pip_p :: Wrapper (Ptr d -> Int -> Ptr b -> IO (Ptr a))
instance CCallable (Ptr d -> Int -> Ptr b -> IO (Ptr a)) where wrapper = _wrapper_pip_p
foreign import ccall "wrapper" _wrapper_ppi_  :: Wrapper (Ptr d -> Ptr c -> Int -> IO ())
instance CCallable (Ptr d -> Ptr c -> Int -> IO ()) where wrapper = _wrapper_ppi_
foreign import ccall "wrapper" _wrapper_ppi_i :: Wrapper (Ptr d -> Ptr c -> Int -> IO Int)
instance CCallable (Ptr d -> Ptr c -> Int -> IO Int) where wrapper = _wrapper_ppi_i
foreign import ccall "wrapper" _wrapper_ppi_p :: Wrapper (Ptr d -> Ptr c -> Int -> IO (Ptr a))
instance CCallable (Ptr d -> Ptr c -> Int -> IO (Ptr a)) where wrapper = _wrapper_ppi_p
foreign import ccall "wrapper" _wrapper_ppp_  :: Wrapper (Ptr d -> Ptr c -> Ptr b -> IO ())
instance CCallable (Ptr d -> Ptr c -> Ptr b -> IO ()) where wrapper = _wrapper_ppp_
foreign import ccall "wrapper" _wrapper_ppp_i :: Wrapper (Ptr d -> Ptr c -> Ptr b -> IO Int)
instance CCallable (Ptr d -> Ptr c -> Ptr b -> IO Int) where wrapper = _wrapper_ppp_i
foreign import ccall "wrapper" _wrapper_ppp_p :: Wrapper (Ptr d -> Ptr c -> Ptr b -> IO (Ptr a))
instance CCallable (Ptr d -> Ptr c -> Ptr b -> IO (Ptr a)) where wrapper = _wrapper_ppp_p
