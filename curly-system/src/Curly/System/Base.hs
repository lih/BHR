{-# LANGUAGE UndecidableInstances, RecursiveDo #-}
module Curly.System.Base where

import Definitive
import Language.Format
import Curly.Core.Annotated
import Curly.Core
import Curly.Core.Library
import IO.Filesystem

newtype RegID = RegID Int
              deriving (Show,Eq,Ord)
data Locus = Register RegID
           | AtOffset Locus Offset
           deriving (Show,Eq,Ord)
data Offset = Offset Int
            | ValueOffset | TypeOffset | EnvOffset
            deriving (Eq,Ord,Show)
data Value = Constant Integer
           | Variable Locus
           deriving (Show,Eq,Ord)

data BinaryCode = BC {
  _bcSize :: Int,
  _bcEstimate :: Int,
  _bData :: Bytes
  }
class BCSerializable t where
  bcEncode :: t -> BinaryCode
                  
newtype BinAddress = BA { getBA :: Int }
                     deriving (Semigroup,Monoid,Eq,Ord)
data Section = TextSection | DataSection | InitSection | RawSection String
             deriving (Eq,Ord,Show)
data Runtime s = Runtime {
  _rtAddresses :: Map (AnnExpr s) BinAddress,
  _rtPartial :: Map Int BinAddress,
  _rtBuiltins :: Map (Section,String) BinAddress,
  _rtSections :: Map Section (BinaryCode,BinAddress)
  }
newtype ASMT s m a = ASMT (StateT (Runtime s) (CounterT BinaryCode BinAddress m) a)
                 deriving (Functor,SemiApplicative,Applicative,Unit,MonadFix
                          ,MonadWriter BinaryCode,MonadCounter BinaryCode BinAddress
                          ,MonadState (Runtime s))
i'ASMT :: Iso (ASMT s m a) (ASMT s' m' a') (StateT (Runtime s) (CounterT BinaryCode BinAddress m) a) (StateT (Runtime s') (CounterT BinaryCode BinAddress m') a') 
i'ASMT = iso ASMT (\(ASMT a) -> a)
runASMT :: MonadFix m => Runtime s -> ASMT s m a -> m (a,Runtime s,BinaryCode)
runASMT rt m = (m^..stateT.i'ASMT) rt^..i'counterT <&> \ ~(~(rt',a),_,bc) -> (a,rt',bc)


newtype Standalone = Standalone { standalone :: forall m s. MonadASM m s => m BinAddress -> m () }
data System = System {
  _sysName :: String,
  _sysProgPerms :: FilePermissions -> FilePermissions,
  _sysStandalone :: forall m s. MonadASM m s => m BinAddress -> m (),
  _sysStandaloneHooks :: Maybe SystemHooks,
  _sysImpl :: SysImpl
  }
rawProgram secs m = mdo
  rtSections =- fromAList [(s',(zero,rt'^.rtSection s.l'2)) | (s,s') <- zip secs (tail secs)]
  ret <- inSection (head secs) m
  rt' <- get
  tell $ fold [rt'^.rtSection s.l'1 | s <- secs]
  return ret
  
rawSections = c'map $ fromAList [(TextSection,BA 1)]
data SysImpl = Imperative (Maybe SystemHooks -> VonNeumannMachine)
             | RawSystem (LeafExpr GlobalID -> Bytes)
instance Eq System where a == b = compare a b == EQ
instance Ord System where
  compare = comparing _sysName
instance Show System where show = _sysName

instance Semigroup BinaryCode where
  ~(BC lo hi d) + ~(BC lo' hi' d') = BC (lo+lo') (hi+hi') (d+d')
instance SubSemi BinAddress BinaryCode where
  cast (BC lo _ _) = BA lo
instance Monoid BinaryCode where
  zero = BC zero zero zero

instance Monad m => Monad (ASMT s m) where join = coerceJoin ASMT
instance MonadTrans (ASMT s) where lift = ASMT . lift . lift
instance MonadReader r m => MonadReader r (ASMT s m) where
  ask = lift ask
  local f = from (mapping i'counterT.stateT.i'ASMT) %~ map (local f)

class (MonadCounter BinaryCode BinAddress m,MonadFix m,MonadState (Runtime s) m) => MonadASM m s | m -> s
instance MonadFix m => MonadASM (ASMT s m) s

t'Register :: Traversal' Locus RegID
t'Register k (Register r) = Register<$>k r
t'Register _ x = return x
t'AtOffset :: Traversal' Locus (Locus,Offset)
t'AtOffset k (AtOffset r o) = uncurry AtOffset<$>k (r,o)
t'AtOffset _ x = return x
t'Variable :: Traversal' Value Locus
t'Variable k (Variable r) = Variable<$>k r
t'Variable _ x = return x

baseRegister :: Locus -> RegID
baseRegister (Register r) = r
baseRegister (AtOffset l _) = baseRegister l

rtAddresses :: Lens (Map (AnnExpr s) BinAddress) (Map (AnnExpr s') BinAddress) (Runtime s) (Runtime s')
rtAddresses = lens _rtAddresses (\x y -> x { _rtAddresses = y })
rtPartial :: Lens' (Runtime s) (Map Int BinAddress)
rtPartial = lens _rtPartial (\x y -> x { _rtPartial = y })
rtBuiltins :: Lens' (Runtime s) (Map (Section,String) BinAddress)
rtBuiltins = lens _rtBuiltins (\x y -> x { _rtBuiltins = y })
rtBuiltin :: Section -> String -> Lens' (Runtime s) BinAddress
rtBuiltin s n = rtBuiltins.at (s,n).l'Just zero
rtSections :: Lens' (Runtime s) (Map Section (BinaryCode,BinAddress))
rtSections = lens _rtSections (\x y -> x { _rtSections = y })
rtSection :: Section -> Lens' (Runtime s) (BinaryCode,BinAddress)
rtSection s = rtSections.at s.l'Just zero

defaultRuntime :: Runtime s
defaultRuntime = Runtime zero zero zero zero

bcSize :: Lens' BinaryCode Int
bcSize = lens _bcSize (\x y -> x { _bcSize = y })
bcEstimate :: Lens' BinaryCode Int
bcEstimate = lens _bcEstimate (\x y -> x { _bcEstimate = y })
bData :: Lens' BinaryCode Bytes
bData = lens _bData (\x y -> x { _bData = y })

binaryCode :: Serializable s => (Maybe Int,Int) -> s -> BinaryCode
binaryCode (mlo,hi) s = bytesCode (mlo,hi) (serialize s)
bytesCode :: (Maybe Int,Int) -> Bytes -> BinaryCode
bytesCode (mlo,hi) bs = BC lo hi bs
  where lo = fromMaybe (bytesSize bs) mlo
bytesCode' :: Bytes -> BinaryCode
bytesCode' bs = let s = bytesSize bs in bytesCode (Just s,s) bs
inSection :: MonadASM m s => Section -> m a -> m a
inSection sec ma = mdo
  cur <- getCounter
  setCounter =<< getl (rtSection sec.l'2)
  rtSection sec =~ \ ~(sbc,_) -> (sbc+bc,end)
  ~(bc,~(a,end)) <- intercept (liftA2 (,) ma getCounter)
  setCounter cur
  return a

type INSTR0            = forall m s. MonadASM m s => m ()
type INSTR1 a          = forall m s. MonadASM m s => a -> m ()
type INSTR2 a b        = forall m s. MonadASM m s => a -> b -> m ()
type INSTR3 a b c      = forall m s. MonadASM m s => a -> b -> c -> m ()
type INSTR4 a b c d    = forall m s. MonadASM m s => a -> b -> c -> d -> m ()
type INSTR5 a b c d e  = forall m s. MonadASM m s => a -> b -> c -> d -> e -> m ()
type BUILTIN_INSTR     = forall m s. MonadASM m s => Builtin -> Maybe (m (BinAddress,Value))
type CCALL             = forall m s. MonadASM m s => Maybe Locus -> BinAddress -> [m Value] -> m ()
type ALLOC_BYTES       = forall m s. MonadASM m s => Locus -> Value -> m ()

data VonNeumannMachine = VonNeumannMachine {
  _destReg,_thisReg,_tmpReg :: RegID,
  _newFunction :: forall m s. MonadASM m s => Section -> m BinAddress,
  _cp,_add :: INSTR2 Locus Value,
  _load :: INSTR2 Locus BinAddress,
  _store :: INSTR2 BinAddress Value,
  _push :: INSTR1 Value,
  _pop :: INSTR1 (Int:+:Locus),
  _pushThunk,_popThunk :: INSTR1 Locus,
  _jcmp :: INSTR5 (Maybe Bool) (Bool,Ordering) Value Value BinAddress, 
  _jmp,_call :: INSTR1 Value,
  _ret :: INSTR0,
  _curlyBuiltin :: BUILTIN_INSTR,
  _assemblyMachine :: Maybe AssemblyMachine
  }
data AssemblyMachine = AssemblyMachine {
  _ccall :: CCALL,
  _poolReg :: RegID,
  _wordSize :: Int,
  _pageSize :: Int
  }
data SystemHooks = SystemHooks {
  _sysPushThunk,_sysPopThunk :: INSTR1 Locus,
  _sysAllocBytes :: ALLOC_BYTES
  }

defBuiltinGet :: (?sys :: VonNeumannMachine, MonadASM m s) => Section -> String -> m () -> m BinAddress
defBuiltinGet sec b m = do
  ma <- getl (rtBuiltins.at (sec,b))
  mute $ case ma of
    Just a -> return a
    Nothing -> mfix $ \a -> do
      rtBuiltins =~ insert (sec,b) a
      inSection sec (newFunction sec <* m)
      
withNewCurlyBuiltins :: BUILTIN_INSTR -> VonNeumannMachine -> VonNeumannMachine
withNewCurlyBuiltins getB m = m { _curlyBuiltin = liftA2 (+) getB (_curlyBuiltin m) }

getArgFun :: (?sys :: VonNeumannMachine,MonadASM m s) => m BinAddress
getArgFun = defBuiltinGet TextSection "argument" $ do 
  pushing [thisReg] $ do
    callThunk (thisReg!ValueOffset)
    tmpReg <-- thisReg
  thisReg <== tmpReg
  ret
getConstantFun :: (?sys :: VonNeumannMachine,MonadASM m s) => m BinAddress
getConstantFun = defBuiltinGet TextSection "constant" $ do
  destReg <== thisReg
  ret
getPartial :: (?sys :: VonNeumannMachine, MonadASM m s) => Int -> m BinAddress
getPartial nbargs = do
  ma <- getl (rtPartial.at nbargs)
  mute $ case ma of
    Just a -> return a
    Nothing -> mfix $ \a -> do
      rtPartial =~ insert nbargs a      
      inSection TextSection $ newFunction TextSection <* do
        -- Inline the current thunk
        let argEnd = composing (const (!EnvOffset)) [0..nbargs] (thisReg!ValueOffset)
        argEnd <-- thisReg!EnvOffset
        
        tmpReg            <-- thisReg!ValueOffset
        thisReg!EnvOffset <-- tmpReg!EnvOffset
        thisReg <== tmpReg

        doTimes nbargs $ do
          call (thisReg!TypeOffset)
          tmpReg              <-- thisReg!EnvOffset
          thisReg!EnvOffset   <-- tmpReg!EnvOffset
          tmpReg!EnvOffset    <-- destReg!ValueOffset
          thisReg!ValueOffset <-- tmpReg
          thisReg!TypeOffset  <-- destReg!TypeOffset

        jmp (thisReg!TypeOffset)

getSeq :: (?sys :: VonNeumannMachine,MonadASM m s) => m BinAddress
getSeq = defBuiltinGet TextSection "seq" $ do
  builtinArgs 2
  pushing [thisReg] $ callThunk (thisReg!ValueOffset!EnvOffset)
  thisReg <-- thisReg!ValueOffset
  jmp (thisReg!TypeOffset)

builtinArgs :: (?sys :: VonNeumannMachine, MonadASM m s) => Int -> m [Locus]
builtinArgs n = reverse (take n (iterate (!EnvOffset) (thisReg!ValueOffset))) <$ ba n
  where ba 0 = unit
        ba n = mdo 
          destReg!TypeOffset <-- end
          destReg!ValueOffset <-- thisReg!ValueOffset
          ret
          end <- newFunction TextSection
          ba (n-1)

commonBuiltin :: (?sys :: VonNeumannMachine) => BUILTIN_INSTR
commonBuiltin B_AddInt = Just $ do
  a <- defBuiltinGet TextSection "addInt" $ mdo
    builtinArgs 2
    pushing [thisReg] $ callThunk (thisReg!ValueOffset)
    pushV (destReg!ValueOffset)
    popThunk (thisReg!ValueOffset)
    pushing [thisReg] $ callThunk (thisReg!ValueOffset)
    popThunk (thisReg!ValueOffset)
    popV tmpReg
    add (destReg!ValueOffset) tmpReg
    cst <- getConstantFun
    thisReg!TypeOffset <-- cst
    thisReg!ValueOffset <-- destReg!ValueOffset
    ret
  return (a,Constant 0)
commonBuiltin (B_Number n) = Just $ do
  cst <- getConstantFun
  return (cst,toValue n)
commonBuiltin (B_FileDesc n) = Just $ do
  cst <- getConstantFun
  return (cst,toValue n)
commonBuiltin B_Unit = Just $ do
  f <- defBuiltinGet TextSection "starting-world" $ ret
  return (f,Constant 0)
commonBuiltin B_Seq = Just $ do
  f <- getSeq
  return (f,Constant 0)
commonBuiltin _ = Nothing

class IsLocus t where
  toLocus :: t -> Locus
instance IsLocus Locus where toLocus = id
instance IsLocus RegID where toLocus = Register
instance IsLocus l => IsLocus (l,Offset) where toLocus = uncurry (!)

class IsValue t where
  toValue :: t -> Value
instance IsValue Value where toValue = id
newtype V t = V t
instance IsLocus l => IsValue (V l) where toValue (V x) = Variable (toLocus x)
instance IsValue Locus where toValue = Variable
instance IsValue RegID where toValue = Variable . toLocus
instance IsValue Integer where toValue = Constant
instance IsValue Int where toValue = Constant . fromIntegral
instance IsValue Word32 where toValue = Constant . fromIntegral
instance IsValue Word8 where toValue = Constant . fromIntegral
instance IsValue BinAddress where toValue (BA a) = Constant (fromIntegral a)

(!) :: IsLocus l => l -> Offset -> Locus
(!) = AtOffset . toLocus
infixl 8 !

(<--) :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l,IsValue v) => l -> v -> m ()
l <-- v = _cp ?sys (toLocus l) (toValue v)
infix 3 <--,<==
setThunkVal :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l,IsValue v,IsValue v') => l -> v -> v' -> m ()
setThunkVal d t v = do
  d!TypeOffset <-- t
  d!ValueOffset <-- v
(<==) :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l,IsLocus l') => l -> l' -> m ()
(<==) r r' = setThunkVal r (r'!TypeOffset) (r'!ValueOffset)

pushV :: (?sys :: VonNeumannMachine,IsValue v) => INSTR1 v
pushV v = _push ?sys (toValue v)
popV :: (?sys :: VonNeumannMachine,IsLocus l) => INSTR1 l
popV l = _pop ?sys (Right (toLocus l))
popN :: (?sys :: VonNeumannMachine) => INSTR1 Int
popN n = _pop ?sys (Left n)
jcmp_hint :: (?sys :: VonNeumannMachine,IsValue v,IsValue v') => INSTR5 (Maybe Bool) (Bool,Ordering) v v' BinAddress
jcmp_hint hint ord v v' = _jcmp ?sys hint ord (toValue v) (toValue v')
ifcmp_hint :: (?sys :: VonNeumannMachine,MonadASM m s,IsValue v,IsValue v') => Maybe Bool -> (Bool,Ordering) -> v -> v' -> m () -> m ()
ifcmp_hint hint ord v v' m = itecmp_hint hint ord v v' m unit
itecmp_hint :: (?sys :: VonNeumannMachine,MonadASM m s,IsValue v,IsValue v') => Maybe Bool -> (Bool,Ordering) -> v -> v' -> m () -> m () -> m ()
itecmp_hint hint ord v v' thenBranch elseBranch = mdo
  if hint==Just False
    then jcmp_hint hint ord v v' =<< inSection TextSection (getCounter <* thenBranch <* jmp end)
    else do jcmp_hint (map not hint) (first not ord) v v' else_
            thenBranch
            let nullEst = w^.bcEstimate==0
            censor (jmp end <&> \() -> ((),\(BC sz est dat) -> BC (if nullEst then 0 else sz) est (if nullEst then zero else dat)))
  else_ <- getCounter
  (w,_) <- listen elseBranch
  end <- getCounter
  return ()
jcmp :: (?sys :: VonNeumannMachine,IsValue v,IsValue v') => INSTR4 (Bool,Ordering) v v' BinAddress
jcmp = jcmp_hint Nothing
ifcmp :: (?sys :: VonNeumannMachine,MonadASM m s,IsValue v,IsValue v') => (Bool,Ordering) -> v -> v' -> m () -> m ()
ifcmp = ifcmp_hint Nothing
itecmp :: (?sys :: VonNeumannMachine,MonadASM m s,IsValue v,IsValue v') => (Bool,Ordering) -> v -> v' -> m () -> m () -> m ()
itecmp = itecmp_hint Nothing
jmp :: (?sys :: VonNeumannMachine,MonadASM m s,IsValue v) => v -> m ()
jmp v = _jmp ?sys (toValue v)
call :: (?sys :: VonNeumannMachine,MonadASM m s,IsValue v) => v -> m ()
call v = _call ?sys (toValue v)
ret :: (?sys :: VonNeumannMachine,MonadASM m s) => m ()
ret = _ret ?sys
tailCall :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus v,IsValue v) => v -> m ()
tailCall v = do thisReg <== v; jmp (thisReg!TypeOffset)
load :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l) => l -> BinAddress -> m ()
load l = _load ?sys (toLocus l)
store :: (?sys :: VonNeumannMachine,MonadASM m s,IsValue v) => BinAddress -> v -> m ()
store a v = _store ?sys a (toValue v)
add :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l,IsValue v) => l -> v -> m ()
add l v = _add ?sys (toLocus l) (toValue v)

assemblyMachine :: (?sys :: VonNeumannMachine) => AssemblyMachine
assemblyMachine = let Just asm = _assemblyMachine ?sys in asm
ccall :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l) => Maybe l -> BinAddress -> [m Value] -> m ()
ccall ml = _ccall assemblyMachine (map toLocus ml)
poolReg :: (?sys :: VonNeumannMachine) => RegID
poolReg = _poolReg assemblyMachine
wordSize :: (?sys :: VonNeumannMachine,Num n) => n
wordSize = fromIntegral (_wordSize assemblyMachine)
pageSize :: (?sys :: VonNeumannMachine,Num n) => n
pageSize = fromIntegral (_pageSize assemblyMachine)
thunkSize :: (?sys :: VonNeumannMachine, Num n, Semiring n) => n
thunkSize = 4*wordSize
pushThunk :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l) => l -> m ()
pushThunk l = _pushThunk ?sys (toLocus l)
popThunk :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l) => l -> m ()
popThunk l = _popThunk ?sys (toLocus l)
callThunk :: (?sys :: VonNeumannMachine,MonadASM m s,IsValue v) => v -> m ()
callThunk val = do
  thisReg <-- val
  call (thisReg!TypeOffset)

newFunction :: (?sys :: VonNeumannMachine,MonadASM m s) => Section -> m BinAddress
newFunction = _newFunction ?sys

callWithStackArgs :: (?sys :: VonNeumannMachine,MonadASM m s,IsValue v,IsValue v') => v -> [m v'] -> m ()
callWithStackArgs f args = do
  traverse_ (\x -> pushV =<< x) args
  call f
  popN (length args)

{- |
The destination register, a pointer to the thunk where the result
of the current computation should be stored.
-}
destReg :: (?sys :: VonNeumannMachine) => RegID
destReg = _destReg ?sys
tmpReg :: (?sys :: VonNeumannMachine) => RegID
tmpReg = _tmpReg ?sys
{-|
The object register, a pointer to the thunk that is currently being
evaluated.
-}
thisReg :: (?sys :: VonNeumannMachine) => RegID
thisReg = _thisReg ?sys

allocBytes :: (?sysHooks :: SystemHooks,IsLocus l,IsValue v,MonadASM m s) => l -> v -> m ()
allocBytes l v = _sysAllocBytes ?sysHooks (toLocus l) (toValue v)

pushing :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l) => [l] -> m a -> m a
pushing l = between (traverse_ (pushV . V) l) (traverse_ popV (reverse l))
rotateL :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l) => [l] -> m ()
rotateL [] = unit
rotateL [_] = unit
rotateL l = sequence_ $ zipWith (<--) (Register tmpReg:map toLocus l) (map toLocus l+[Register tmpReg])

align :: MonadASM m s => Int -> Word8 -> m ()
align n c = do
  BA cur <- getCounter
  let padSize = nn - ((cur + nn) `mod` n)
      nn = n-1
  tell (bytesCode (Just padSize,nn) (pack (take padSize (repeat c))))
reserve :: MonadASM m s => Int -> Word8 -> m ()
reserve n c = tell (bytesCode (Just n,n) (pack (take n (repeat c))))

