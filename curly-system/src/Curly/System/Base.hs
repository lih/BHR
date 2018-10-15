{-# LANGUAGE UndecidableInstances, RecursiveDo, LambdaCase, ViewPatterns #-}
module Curly.System.Base where

import Definitive
import Language.Format
import Curly.Core.Annotated
import Curly.Core
import Curly.Core.Library
import IO.Filesystem
import Curly.Core.Documentation

newtype RegID = RegID Int
              deriving (Show,Eq,Ord)
newtype BinAddress = BA { getBA :: Int }
                     deriving (Semigroup,Monoid,Eq,Ord)
data OffsetStride = ByteStride RegID | WordStride RegID | NoStride
                  deriving (Eq,Ord,Show)
data Locus = Register RegID
           | AtOffset Locus OffsetStride Offset 
           deriving (Show,Eq,Ord)
data Offset = Offset Int
            | ValueOffset | TypeOffset | EnvOffset
            deriving (Eq,Ord,Show)
data Value = Constant Integer
           | Variable Locus
           deriving (Show,Eq,Ord)

version = 1 :: Int

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

(!%) :: IsLocus l => l -> (OffsetStride,Offset) -> Locus
l !% (i,o) = AtOffset (toLocus l) i o
(!) :: IsLocus l => l -> Offset -> Locus
l ! o = l !% (NoStride,o)
infixl 8 !, !%

t'Register :: Traversal' Locus RegID
t'Register k (Register r) = Register<$>k r
t'Register _ x = return x
t'AtOffset :: Traversal' Locus (Locus,OffsetStride,Offset)
t'AtOffset k (AtOffset r i o) = uncurry3 AtOffset<$>k (r,i,o)
t'AtOffset _ x = return x
t'Variable :: Traversal' Value Locus
t'Variable k (Variable r) = Variable<$>k r
t'Variable _ x = return x

baseRegister :: Locus -> RegID
baseRegister (Register r) = r
baseRegister (AtOffset l _ _) = baseRegister l

data BinaryCode = BC {
  _bcSize :: Int,
  _bcEstimate :: Int,
  _bData :: Bytes
  }
instance Semigroup BinaryCode where
  ~(BC lo hi d) + ~(BC lo' hi' d') = BC (lo+lo') (hi+hi') (d+d')
instance SubSemi BinAddress BinaryCode where
  cast (BC lo _ _) = BA lo
instance Monoid BinaryCode where
  zero = BC zero zero zero
class BCSerializable t where
  bcEncode :: t -> BinaryCode
 
bcSize :: Lens' BinaryCode Int
bcSize = lens _bcSize (\x y -> x { _bcSize = y })
bcEstimate :: Lens' BinaryCode Int
bcEstimate = lens _bcEstimate (\x y -> x { _bcEstimate = y })
bData :: Lens' BinaryCode Bytes
bData = lens _bData (\x y -> x { _bData = y })

binaryCode :: Serializable Word8 Builder Bytes s => (Maybe Int,Int) -> s -> BinaryCode
binaryCode (mlo,hi) s = bytesCode (mlo,hi) (serialize s)
bytesCode :: (Maybe Int,Int) -> Bytes -> BinaryCode
bytesCode (mlo,hi) bs = BC lo hi bs
  where lo = fromMaybe (bytesSize bs) mlo
bytesCode' :: Bytes -> BinaryCode
bytesCode' bs = let s = bytesSize bs in bytesCode (Just s,s) bs

data Section = TextSection | DataSection | InitSection | RawSection String
             deriving (Eq,Ord,Show)
data Runtime s = Runtime {
  _rtAddresses :: Map (AnnExpr s) BinAddress,
  _rtPartial :: Map Int BinAddress,
  _rtBuiltins :: Map (Section,String) BinAddress,
  _rtRawSections :: Map Hash BinAddress,
  _rtSections :: Map Section (BinaryCode,BinAddress),
  _rtDirty :: Map Int Bool
  }
 
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
rtDirty :: Int -> Lens' (Runtime s) Bool
rtDirty reg = lens _rtDirty (\x y -> x { _rtDirty = y }).mat reg

defaultRuntime :: Runtime s
defaultRuntime = Runtime zero zero zero zero zero zero

newtype ASMT s m a = ASMT (StateT (Runtime s) (CounterT BinaryCode BinAddress m) a)
                 deriving (Functor,SemiApplicative,Applicative,Unit,MonadFix
                          ,MonadWriter BinaryCode,MonadCounter BinaryCode BinAddress
                          ,MonadState (Runtime s))
instance Monad m => Monad (ASMT s m) where join = coerceJoin ASMT
instance MonadTrans (ASMT s) where lift = ASMT . lift . lift
instance MonadReader r m => MonadReader r (ASMT s m) where
  ask = lift ask
  local f = from (mapping i'counterT.stateT.i'ASMT) %~ map (local f)


class (MonadCounter BinaryCode BinAddress m,MonadFix m,MonadState (Runtime s) m) => MonadASM m s | m -> s
instance MonadFix m => MonadASM (ASMT s m) s
 
i'ASMT :: Iso (ASMT s m a) (ASMT s' m' a') (StateT (Runtime s) (CounterT BinaryCode BinAddress m) a) (StateT (Runtime s') (CounterT BinaryCode BinAddress m') a') 
i'ASMT = iso ASMT (\(ASMT a) -> a)
runASMT :: MonadFix m => Runtime s -> ASMT s m a -> m (a,Runtime s,BinaryCode)
runASMT rt m = (m^..stateT.i'ASMT) rt^..i'counterT <&> \ ~(~(rt',a),_,bc) -> (a,rt',bc)

align :: MonadASM m s => Int -> Word8 -> m ()
align n c = do
  BA cur <- getCounter
  let padSize = nn - ((cur + nn) `mod` n)
      nn = n-1
  tell (bytesCode (Just padSize,nn) (pack (take padSize (repeat c))))
reserve :: MonadASM m s => Int -> Word8 -> m ()
reserve n c = tell (bytesCode (Just n,n) (pack (take n (repeat c))))
inSection :: MonadASM m s => Section -> m a -> m a
inSection sec ma = mdo
  cur <- getCounter
  setCounter =<< getl (rtSection sec.l'2)
  rtSection sec =~ \ ~(sbc,_) -> (sbc+bc,end)
  ~(bc,~(a,end)) <- intercept (liftA2 (,) ma getCounter)
  setCounter cur
  return a

rawProgram secs m = mdo
  rtSections =- fromAList [(s',(zero,rt'^.rtSection s.l'2)) | (s,s') <- zip secs (tail secs)]
  ret <- inSection (head secs) m
  rt' <- get
  tell $ fold [rt'^.rtSection s.l'1 | s <- secs]
  return ret
rawSections = c'map $ fromAList [(TextSection,BA 1)]
 
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
  _cp,_add,_sub :: INSTR2 Locus Value,
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
  _cret :: RegID,
  _poolReg :: RegID,
  _wordSize :: Int,
  _pageSize :: Int
  }
data SystemHooks = SystemHooks {
  _sysPushThunk,_sysPopThunk :: INSTR1 Locus,
  _sysAllocBytes :: ALLOC_BYTES
  }

getOrDefine :: (?sys :: VonNeumannMachine, MonadASM m s) => Section -> String -> m () -> m BinAddress
getOrDefine sec b m = do
  ma <- getl (rtBuiltins.at (sec,b))
  mute $ case ma of
    Just a -> return a
    Nothing -> mfix $ \a -> do
      rtBuiltins =~ insert (sec,b) a
      inSection sec (newFunction sec <* m)
getOrDefineBuiltin :: (?sys :: VonNeumannMachine, MonadASM m s) => Section -> String -> Value -> m () -> m (BinAddress,Value)
getOrDefineBuiltin sec b v m = (,v) <$> getOrDefine sec b m
getOrDefineBuiltin0 :: (?sys :: VonNeumannMachine, MonadASM m s) => Section -> String -> m () -> m (BinAddress,Value)
getOrDefineBuiltin0 sec b m = getOrDefineBuiltin sec b (Constant 0) m
globalBuiltin :: (?sys :: VonNeumannMachine, MonadASM m s) => m BinAddress -> Value -> m (BinAddress,Value)
globalBuiltin ma v = map (,v) ma
withAdditionalBuiltins :: BUILTIN_INSTR -> VonNeumannMachine -> VonNeumannMachine
withAdditionalBuiltins getB m = m { _curlyBuiltin = liftA2 (+) getB (_curlyBuiltin m) }
 
global_argFun :: (?sys :: VonNeumannMachine,MonadASM m s) => m BinAddress
global_argFun = getOrDefine TextSection "argument" $ do 
  pushing [thisReg] $ do
    callThunk (thisReg!ValueOffset)
    tmpReg <-- thisReg
  thisReg <== tmpReg
  ret
global_constant :: (?sys :: VonNeumannMachine,MonadASM m s) => m BinAddress
global_constant = getOrDefine TextSection "constant" $ do
  destReg <== thisReg
  ret
global_partialApply :: (?sys :: VonNeumannMachine, MonadASM m s) => Int -> m BinAddress
global_partialApply nbargs = do
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

global_seq :: (?sys :: VonNeumannMachine,MonadASM m s) => m BinAddress
global_seq = getOrDefine TextSection "seq" $ do
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
commonBuiltin B_AddInt = Just $ getOrDefineBuiltin0 TextSection "addInt" $ mdo
  builtinArgs 2
  pushing [thisReg] $ callThunk (thisReg!ValueOffset)
  pushV (destReg!ValueOffset)
  popThunk (thisReg!ValueOffset)
  pushing [thisReg] $ callThunk (thisReg!ValueOffset)
  popThunk (thisReg!ValueOffset)
  popV tmpReg
  add (destReg!ValueOffset) tmpReg
  cst <- global_constant
  thisReg!TypeOffset <-- cst
  thisReg!ValueOffset <-- destReg!ValueOffset
  ret
commonBuiltin (B_Number n) = Just $ globalBuiltin global_constant (toValue n)
commonBuiltin (B_FileDesc n) = Just $ globalBuiltin global_constant (toValue n)
commonBuiltin B_Unit = Just $ getOrDefineBuiltin0 TextSection "unit" $ ret
commonBuiltin B_Seq = Just $ globalBuiltin global_seq (Constant 0)
commonBuiltin B_CmpInt_LT = Just $ getOrDefineBuiltin0 TextSection "cmpInt_lt" $ do
  [n,m] <- builtinArgs 2
  pushing [thisReg] $ callThunk n
  pushing [thisReg] $ callThunk m
  itecmp (True,LT) (n!ValueOffset) (m!ValueOffset)
    (do [th,_] <- builtinArgs 2
        tailCall th)
    (do [_,el] <- builtinArgs 2
        tailCall el)
commonBuiltin B_CmpInt_EQ = Just $ getOrDefineBuiltin0 TextSection "cmpInt_eq" $ do
  [n,m] <- builtinArgs 2
  pushing [thisReg] $ callThunk n
  pushing [thisReg] $ callThunk m
  itecmp (True,EQ) (n!ValueOffset) (m!ValueOffset)
    (do [th,_] <- builtinArgs 2
        tailCall th)
    (do [_,el] <- builtinArgs 2
        tailCall el)
commonBuiltin _ = Nothing

data SystemDataRepr = SystemDataRepr {
  sdr_encodeWord16 :: Word16 -> Builder,
  sdr_encodeWord32 :: Word32 -> Builder,
  sdr_encodeWord64 :: Word64 -> Builder,
  sdr_encodeWordN :: Word32 -> Builder,
  sdr_byteOrder :: Bool,
  sdr_wordSize :: Int
  }

tellWordN repr n = tell $ bytesCode' (sdr_encodeWordN repr n^..bytesBuilder)
assemblyBuiltin :: (?sysHooks :: SystemHooks, ?sys :: VonNeumannMachine) => SystemDataRepr -> BUILTIN_INSTR
assemblyBuiltin repr (B_String s) = Just $ do
  str <- inSection DataSection $ getCounter <* do
    tellWordN repr 1
    tellWordN repr (fromIntegral (length s))
    for_ s $ tell . binaryCode (Just 1,1)
  globalBuiltin global_constant (toValue str)
assemblyBuiltin repr B_AddString = Just $ getOrDefineBuiltin0 TextSection "add-string" $ do
  [a,b] <- builtinArgs 2
  pushing [thisReg] $ callThunk a
  pushing [thisReg] $ callThunk b
  tmpReg <-- a!ValueOffset!Offset wordSize
  add tmpReg (b!ValueOffset!Offset wordSize)
  add tmpReg (2*wordSize :: Int)

  allocBytes (thisReg!ValueOffset) tmpReg
  
  sub tmpReg (2*wordSize :: Int)
  thisReg!ValueOffset!Offset 0 <-- (1 :: Int)
  thisReg!ValueOffset!Offset wordSize <-- tmpReg
  let fillLoop v = do
        start <- getCounter
        tmpReg <-- (0 :: Int)
        ifcmp_hint (Just True) (True,LT) tmpReg v $ do
          thisReg!ValueOffset!%(ByteStride tmpReg,Offset 0) <-- v
          add tmpReg (wordSize :: Int)
  between
    (add (thisReg!ValueOffset) (2*wordSize :: Int))
    (sub (thisReg!ValueOffset) (2*wordSize :: Int)) $ do
      fillLoop (a!ValueOffset)
      between
        (add (thisReg!ValueOffset) (a!ValueOffset))
        (sub (thisReg!ValueOffset) (a!ValueOffset)) $ do
          fillLoop (b!ValueOffset)

  cst <- global_constant
  thisReg!TypeOffset <-- cst
  jmp cst
assemblyBuiltin repr (B_Bytes bs) = Just $ do
  str <- inSection DataSection $ getCounter <* do
    tellWordN repr 1
    tellWordN repr (fromIntegral (bytesSize bs))
    tell $ bytesCode' bs
  globalBuiltin global_constant (toValue str)
assemblyBuiltin _ B_MkArray = Just $ getOrDefineBuiltin0 TextSection "mkArray" $ do
  [size] <- builtinArgs 1
  tmpReg <-- size
  doTimes (case wordSize of 8 -> 3; _ -> 2) $ add tmpReg tmpReg
  add tmpReg (2*wordSize :: Int)
  allocBytes tmpReg tmpReg
  tmpReg!Offset 0 <-- (1::Int)
  tmpReg!Offset wordSize <-- size
  cst <- global_constant
  thisReg!TypeOffset <-- cst
  thisReg!ValueOffset <-- tmpReg
  jmp cst
assemblyBuiltin _ B_ArrayAt = Just $ getOrDefineBuiltin0 TextSection "arrayGet" $ do
  [arr,ind] <- builtinArgs 2
  pushing [thisReg] $ callThunk arr
  pushing [thisReg] $ callThunk ind
  tmpReg <-- destReg!ValueOffset
  cst <- global_constant
  thisReg!ValueOffset <-- arr!ValueOffset!%(WordStride tmpReg,Offset (2*wordSize))
  thisReg!TypeOffset <-- cst
  jmp cst
assemblyBuiltin _ B_ArraySet = Just $ getOrDefineBuiltin0 TextSection "arraySet" $ do
  [arr,ind,v,k] <- builtinArgs 4
  pushing [thisReg] $ callThunk arr
  pushing [thisReg] $ callThunk ind
  tmpReg <-- destReg!ValueOffset
  arr!ValueOffset!%(WordStride tmpReg,Offset (2*wordSize)) <-- v
  tailCall k
assemblyBuiltin _ B_ArrayLength = Just $ getOrDefineBuiltin0 TextSection "arrayLength" $ do
  [arr] <- builtinArgs 1
  pushing [thisReg] $ callThunk arr
  cst <- global_constant
  thisReg!ValueOffset <-- arr!ValueOffset!Offset wordSize
  thisReg!TypeOffset <-- cst
  jmp cst
assemblyBuiltin _ _ = Nothing

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
sub :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l,IsValue v) => l -> v -> m ()
sub l v = _sub ?sys (toLocus l) (toValue v)

assemblyMachine :: (?sys :: VonNeumannMachine) => AssemblyMachine
assemblyMachine = let Just asm = _assemblyMachine ?sys in asm
ccall_void :: Maybe Locus
ccall_void = Nothing
ccall :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l) => Maybe l -> BinAddress -> [m Value] -> m ()
ccall ml = _ccall assemblyMachine (map toLocus ml)
ccall0 :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l) => Maybe l -> BinAddress -> m ()
ccall0 ml a = ccall ml a []
ccall1 :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l,IsValue v) => Maybe l -> BinAddress ->
          m v -> m ()
ccall1 ml a v1 = ccall ml a [map toValue v1]
ccall2 :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l,IsValue v,IsValue v') => Maybe l -> BinAddress ->
          m v -> m v' -> m ()
ccall2 ml a v1 v2 = ccall ml a [map toValue v1, map toValue v2]
ccall3 :: (?sys :: VonNeumannMachine,MonadASM m s,IsLocus l,IsValue v,IsValue v',IsValue v'') => Maybe l -> BinAddress ->
          m v -> m v' -> m v'' -> m ()
ccall3 ml a v1 v2 v3 = ccall ml a [map toValue v1, map toValue v2, map toValue v3]
cret :: (?sys :: VonNeumannMachine) => RegID
cret = _cret assemblyMachine

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

setDest :: (?sys :: VonNeumannMachine,IsValue v,IsValue v',MonadASM m s) => v -> v' -> m ()
setDest t v = do
  destReg!TypeOffset  <-- t
  destReg!ValueOffset <-- v

specialize :: forall m. (?sys :: VonNeumannMachine,MonadASM m GlobalID) => String -> AnnExpr GlobalID -> m BinAddress
specialize sysName expr = inSection TextSection $ getCounter <* specTail (sem expr)
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
    specHead (SemSymbol (Builtin _ (B_Foreign defs def))) = uncurry setDest =<< foreignBuiltin sysName def defs
      where foreignBuiltin sys def defs = case findSym i of
              Just le -> globalBuiltin (specialize sys (anonymous (le^.leafVal))) (Constant 0)
              Nothing -> error $ format "Couldn't find link destination for symbol: %s" (show i)
              where i = fromMaybe def (lookup sys defs)
    specHead (SemSymbol (Builtin _ b)) = case _curlyBuiltin ?sys b of
      Just mav -> uncurry setDest =<< mav
      Nothing -> error $ format "The builtin %s is not yet implemented on this system." (show b)
    specHead (SemAbstract _ body) = do
      a <- specLambda body
      setDest a (if empty (exprRefs body) then toValue (0 :: Int) else toValue (thisReg!ValueOffset))
    specHead (SemApply
              (sem -> SemSymbol (Builtin _ (B_RawIndex i)))
              (sem -> SemSymbol (Builtin _ b))) = case _curlyBuiltin ?sys b of
      Just mav -> do
        (a,v) <- (`map`mav) $ second $ \case
          Constant n -> Constant (n+fromIntegral i)
          v -> v
        setDest a v
      Nothing -> error $ format "Couldn't find implementation for builtin %s" (show b)
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

newtype Standalone = Standalone { standalone :: forall m s. MonadASM m s => m BinAddress -> m () }
data SysImpl = Imperative (Maybe SystemHooks -> VonNeumannMachine)
             | RawSystem (LeafExpr GlobalID -> Bytes)
data System = System {
  _sysName :: String,
  _sysProgPerms :: FilePermissions -> FilePermissions,
  _sysStandalone :: Standalone,
  _sysStandaloneHooks :: Maybe SystemHooks,
  _sysImpl :: SysImpl
  }
instance Eq System where a == b = compare a b == EQ
instance Ord System where
  compare = comparing _sysName
instance Show System where show = _sysName


