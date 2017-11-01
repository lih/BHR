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

specialize :: forall m s. (?sys :: VonNeumannMachine,MonadASM m s,Show s,Show (Pretty s),Identifier s) => AnnExpr s -> m BinAddress
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
      f <- getArgFun
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
              szth <- getPartial (length l)
              setDest szth tmpReg


specializeStandalone :: System -> LeafExpr GlobalID -> Bytes
specializeStandalone sys e = let ?sys = sys in
  let Id (_,_,bin) = runASMT defaultRuntime $ do
        _sysStandalone sys $ case _sysImpl sys of
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
        print fp
        runIOFunPtr fp
  return runIt

foreign import ccall "wrapper" get_malloc_fptr :: (Int -> IO (Ptr a)) -> IO (FunPtr (Int -> IO (Ptr a)))
foreign import ccall "wrapper" get_debug_fptr :: IO () -> IO (FunPtr (IO ()))
foreign import ccall "dynamic" runIOFunPtr :: FunPtr (IO ()) -> IO ()
mallocAddr :: BinAddress
mallocAddr = BA (fromIntegral (ptrToIntPtr (castFunPtrToPtr mallocPtr)))
  where mallocPtr = get_malloc_fptr mallocBytes^.thunk
debug_addr :: IO () -> BinAddress
debug_addr m = BA $ fromIntegral $ ptrToIntPtr $ castFunPtrToPtr $ get_debug_fptr m^.thunk

jit_memextend_pool sz = defBuiltinGet TextSection ("memextend-pool-"+show sz) $ do
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

jit_machine :: VonNeumannMachine
jit_machine = let Imperative imp = _sysImpl hostSystem
              in imp $ Just $ SystemHooks jit_pushThunk jit_popThunk jit_allocBytes
newJITContext :: IO (JITContext s)
newJITContext = map JITContext (newIORef (JITData defaultRuntime zero))
jitExpr :: (Show (Pretty s),Identifier s) => JITContext s -> AnnExpr s -> IO RunJITExpr
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
