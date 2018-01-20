{-# LANGUAGE RecursiveDo, NoMonomorphismRestriction #-}
module Curly.System.X86.Common where

import Definitive
import Language.Format
import Data.Bits
import Curly.System.Base
import Curly.Core
import IO.Filesystem

notImplemented :: a
notImplemented = error "Not implemented"

data X86 = X86 | X86_64
if32 :: (?x86 :: X86) => a -> a -> a
if32 a b = case ?x86 of
  X86 -> a
  X86_64 -> b

encodeWord :: (?x86 :: X86) => Word32 -> BinaryCode
encodeWord w = if32 (bcEncode (LittleEndian w)) (bcEncode (LittleEndian (fromIntegral w :: Word64)))

x86_wordSize :: (?x86 :: X86, Num n) => n
x86_wordSize = if32 4 8

archOffset :: (?x86 :: X86, Num n, Semiring n) => Offset -> n
archOffset (Offset n) = fromIntegral n
archOffset ValueOffset = 3*x86_wordSize
archOffset TypeOffset = 2*x86_wordSize
archOffset EnvOffset = x86_wordSize

data ModRM = NoDisp R RM0
           | Disp8  R RM Word8
           | Disp32 R RM Word32
           | Value  R R
           | NoModRM
           deriving Show
data R = R_eax | R_ecx | R_edx | R_ebx | R_esp_ah | R_ebp_ch | R_esi_dh | R_edi_bh
       | R_r8 | R_r9 | R_r10 | R_r11 | R_r12 | R_r13 | R_r14 | R_r15
              deriving (Show,Enum)
data RM = RM_eax | RM_ecx | RM_edx | RM_ebx | RM_sib SIB | RM_ebp | RM_esi | RM_edi
        | RM_r8 | RM_r9 | RM_r10 | RM_r11 | RM_r12 | RM_r13 | RM_r14 | RM_r15
        deriving Show

instance IsLocus R where toLocus = Register . reg
instance IsValue R where toValue = Variable . toLocus

rm_ind RM_eax = 0 ; rm_ind RM_ecx = 1 ; rm_ind RM_edx = 2 ; rm_ind RM_ebx = 3
rm_ind (RM_sib _) = 4 ; rm_ind RM_ebp = 5 ; rm_ind RM_esi = 6 ; rm_ind RM_edi = 7
rm_ind RM_r8 = 8 ; rm_ind RM_r9 = 9 ; rm_ind RM_r10 = 10 ; rm_ind RM_r11 = 11
rm_ind RM_r12 = 12 ; rm_ind RM_r13 = 13 ; rm_ind RM_r14 = 14 ; rm_ind RM_r15 = 11

data RM0 = RM0_eax | RM0_ecx | RM0_edx | RM0_ebx | RM0_sib SIB | RM0_disp32 Word32 | RM0_esi | RM0_edi
         | RM0_r8 | RM0_r9 | RM0_r10 | RM0_r11 | RM0_r12 | RM0_r13 | RM0_r14 | RM0_r15
        deriving Show

rm0_ind RM0_eax = 0 ; rm0_ind RM0_ecx = 1 ; rm0_ind RM0_edx = 2 ; rm0_ind RM0_ebx = 3
rm0_ind (RM0_sib _) = 4 ; rm0_ind (RM0_disp32 _) = 5 ; rm0_ind RM0_esi = 6 ; rm0_ind RM0_edi = 7
rm0_ind RM0_r8 = 8 ; rm0_ind RM0_r9 = 9 ; rm0_ind RM0_r10 = 10 ; rm0_ind RM0_r11 = 11
rm0_ind RM0_r12 = 12 ; rm0_ind RM0_r13 = 13 ; rm0_ind RM0_r14 = 14 ; rm0_ind RM0_r15 = 11

data SIB = SIB Scale Index Base
           deriving Show
data Scale = SC_1 | SC_2 | SC_4 | SC_8
           deriving (Enum,Show)
data Index = I_eax | I_ecx | I_edx | I_ebx | I_none | I_ebp | I_esi | I_edi
           | I_r8 | I_r9 | I_r10 | I_r11 | I_r12 | I_r13 | I_r14 | I_r15
           deriving (Enum,Show)
data Base = B_eax | B_ecx | B_edx | B_ebx | B_esp | B_none | B_esi | B_edi
          | B_r8 | B_r9 | B_r10 | B_r11 | B_r12 | B_r13 | B_r14 | B_r15
          deriving (Enum,Show)

data OpCode = OpCode [Word8] (Maybe (Word8,R))
            deriving Show
data Prefix = BranchTaken | BranchNotTaken | REX Bool Bool Bool Bool
            deriving (Show)
instance Eq Prefix where a==b = compare a b == EQ
instance Ord Prefix where
  compare = comparing t
    where t BranchNotTaken = 0 ; t BranchTaken = 1 ; t (REX _ _ _ _) = 2
t'REX :: Traversal' Prefix (Bool,Bool,Bool,Bool)
t'REX = prism f g
  where f (REX w r x b) = Right (w,r,x,b)
        f x = Left x
        g _ (w,r,x,b) = REX w r x b

data Instruction = Instruction (Set Prefix) OpCode Immediate ModRM
                 deriving Show
data Immediate = Imm0 
               | Imm8 Word8
               | Imm32 Word32
               | Imm64 Word64
                 deriving Show

en :: Enum n => n -> Word8
en = fi . fromEnum
fi :: (Num n,Integral m) => m -> n
fi = fromIntegral
bcWord8 = BC 1 1 . yb bytesBuilder . word8
instance BCSerializable Word32 where bcEncode = binaryCode (Just 4,4)
instance BCSerializable Word64 where bcEncode = binaryCode (Just 8,8)
instance BCSerializable (LittleEndian Word32) where bcEncode = binaryCode (Just 4,4)
instance BCSerializable (LittleEndian Word64) where bcEncode = binaryCode (Just 8,8)
instance BCSerializable Word8 where bcEncode = bcWord8
instance BCSerializable Prefix where
  bcEncode BranchNotTaken = bcWord8 0x2e
  bcEncode BranchTaken = bcWord8 0x3e
  bcEncode (REX w r x b) = bcWord8 (0x40 .|. en b .|. en x`shiftL`1 .|. en r`shiftL`2 .|. en w`shiftL`3)
instance BCSerializable SIB where
  bcEncode (SIB s i b) = bcWord8 (en s`shiftL`6 .|. (en i .&. 7)`shiftL`3 .|. (en b.&.7))
instance BCSerializable ModRM where
  bcEncode = bcEncode'
    where bcEncode' (NoDisp r (RM0_sib sib)) = modrm 0 r 4+bcEncode sib
          bcEncode' (NoDisp r (RM0_disp32 d)) = modrm 0 r 5+bcEncode (LittleEndian d)
          bcEncode' (NoDisp r rm) = modrm 0 r (rm0_ind rm)
          bcEncode' (Disp8 r (RM_sib sib) d) = modrm 1 r 4+bcEncode sib+bcEncode d
          bcEncode' (Disp8 r rm d) = modrm 1 r (rm_ind rm)+bcEncode d
          bcEncode' (Disp32 r (RM_sib sib) d) = modrm 2 r 4+bcEncode sib+bcEncode (LittleEndian d)
          bcEncode' (Disp32 r rm d) = modrm 2 r (rm_ind rm)+bcEncode (LittleEndian d)
          bcEncode' (Value r rm) = modrm 3 r (fromEnum rm)
          bcEncode' NoModRM = zero
          modrm mod r rm = bcWord8 ((fi rm .&. 7) .|. (en r .&. 7)`shiftL`3 .|. mod`shiftL`6)
instance BCSerializable OpCode where
  bcEncode (OpCode cs r) = foldMap bcEncode cs + maybe zero enc r
    where enc (c,r) = bcWord8 (c .|. (en r .&. 7))
instance BCSerializable Immediate where
  bcEncode Imm0 = zero
  bcEncode (Imm8 w) = bcWord8 w
  bcEncode (Imm32 d) = bcEncode (LittleEndian d)
  bcEncode (Imm64 q) = bcEncode (LittleEndian q)
instance BCSerializable Instruction where
  bcEncode (Instruction p c i m) = foldMap bcEncode p'+bcEncode c+bcEncode m+bcEncode i
    where p' = c'set (withPref p) <&> t'REX %~ \(w,_,_,_) -> (w,r' >= 8,x',b')
          withPref | x' || b' || r'>=8 = touch (REX True False False False)
                   | otherwise = id
          (x',b') = case rm' of
            Left n -> (False,n>=8)
            Right (i,b) -> (fromEnum i>=8,fromEnum b>=8)
          (r',rm') = case m of
            NoModRM -> case c of
              OpCode _ (Just (_,r)) -> (0,Left (fromEnum r))
              _ -> (0,Left 0)
            NoDisp r rm0 -> (fromEnum r,
                             case rm0 of RM0_sib (SIB _ i b) -> Right (i,b)
                                         _ -> Left (rm0_ind rm0))
            Disp8 r rm _ -> (fromEnum r,f rm)
            Disp32 r rm _ -> (fromEnum r,f rm)
            Value a b -> (fromEnum a,Left (fromEnum b))
          f (RM_sib (SIB _ i b)) = Right (i,b)
          f x = Left (rm_ind x)

toRM (RegID rb) i = case i of
  NoStride -> toRM rb
  ByteStride (RegID ri) -> toRM_I rb SC_1 ri
  WordStride (RegID ri) -> toRM_I rb (if32 SC_4 SC_8) ri
  where toRM 0 = RM_eax
        toRM 1 = RM_ecx
        toRM 2 = RM_edx
        toRM 3 = RM_ebx
        toRM 4 = RM_sib (SIB SC_1 I_none B_esp)
        toRM 5 = RM_ebp
        toRM 6 = RM_esi
        toRM 7 = RM_edi
        toRM 8 = RM_r8
        toRM 9 = RM_r9
        toRM 10 = RM_r10
        toRM 11 = RM_r11
        toRM 12 = RM_r12
        toRM 13 = RM_r13
        toRM 14 = RM_r14
        toRM 15 = RM_r15
        toRM n = error $ "Invalid register index "+show n

        toRM_I rb sc ri = RM_sib (SIB sc (toI ri) (toB rb))

        toI r | inRange 0 3 r || inRange 5 15 r = toEnum r
              | otherwise = error $ "Invalid index register"+show r
        toB r | inRange 0 4 r || inRange 6 15 r = toEnum r
              | otherwise = error $ "Invalid base register "+show r

toR (RegID r) = toEnum r

tellBC = tell . foldMap bcEncode

x86_prefix :: (?x86 :: X86) => Set Prefix
x86_prefix = if32 zero (singleton' (REX True False False False))

immN n = if32 (Imm32 (fromInteger n)) (Imm64 (fromInteger n))

rsrc = if32 (RegID 6) (RegID 9)
rdst = if32 (RegID 7) (RegID 10)

x86_cp (Register r)              (Constant i)
  = tellBC [Instruction x86_prefix (OpCode [] (Just (0xb8,toR r))) (immN i) NoModRM]
x86_cp (Register rd)              (Variable (Register rs))
  = tellBC [Instruction x86_prefix (OpCode [0x89] Nothing) Imm0 (Value (toR rs) (toR rd))]
x86_cp (Register rd)              (Variable (AtOffset (Register rb) i o))
  = tellBC [Instruction x86_prefix (OpCode [0x8b] Nothing) Imm0 (Disp8 (toR rd) (toRM rb i) (archOffset o))]
x86_cp (Register r)              (Variable (AtOffset l i o))
  = do x86_cp (Register rsrc) (Variable l)
       x86_cp (Register r) (Variable (rsrc !% (i,o)))
x86_cp (AtOffset (Register r) i o) (Constant n)
  = if32
    (tellBC [Instruction x86_prefix (OpCode [0xc7] Nothing) (Imm32 (fromInteger n)) (Disp8 (toEnum 0) (toRM r i) (archOffset o))])
    (x86_cp (Register rsrc) (Constant n) >> x86_cp (r !% (i,o)) (Variable (Register rsrc)))
x86_cp (AtOffset (Register r) i o) (Variable (Register r'))
  = tellBC [Instruction x86_prefix (OpCode [0x89] Nothing) Imm0 (Disp8 (toR r') (toRM r i) (archOffset o))]
x86_cp (AtOffset (Register r) i o) (Variable l)
  = do x86_cp (Register rsrc) (Variable l)
       x86_cp (r!%(i,o)) (Variable (Register rsrc))
x86_cp (AtOffset l i o)                    v
  = do x86_cp (Register rdst) (Variable l)
       x86_cp (rdst!%(i,o)) v

x86_add (Register r) (Constant i)
  = tellBC [Instruction x86_prefix (OpCode [0x81] Nothing) (Imm32 (fromInteger i)) (Value (toEnum 0) (toR r))]
x86_add (Register r) (Variable (Register r'))
  = tellBC [Instruction x86_prefix (OpCode [0x01] Nothing) Imm0 (Value (toR r') (toR r))]
x86_add (Register rd) (Variable (AtOffset (Register rb) i o))
  = tellBC [Instruction x86_prefix (OpCode [0x03] Nothing) Imm0 (Disp8 (toR rd) (toRM rb i) (archOffset o))]
x86_add (Register r) (Variable (AtOffset l i o))
  = do x86_cp (Register rsrc) (Variable l)
       x86_add (Register r) (Variable (rsrc!%(i,o)))
x86_add (AtOffset (Register r) i o) (Constant n)
  = tellBC [Instruction x86_prefix (OpCode [0x81] Nothing) (Imm32 (fromInteger n)) (Disp8 (toEnum 0) (toRM r i) (archOffset o))]
x86_add (AtOffset (Register r) i o) (Variable (Register r'))
  = tellBC [Instruction x86_prefix (OpCode [0x01] Nothing) Imm0 (Disp8 (toR r') (toRM r i) (archOffset o))]
x86_add (AtOffset (Register r) i o) (Variable (AtOffset r' i' o'))
  = do x86_cp (Register rsrc) (Variable (r'!%(i',o')))
       x86_add (r!%(i,o)) (Variable (Register rsrc))
x86_add (AtOffset l i o) v
  = do x86_cp (Register rdst) (Variable l)
       x86_add (rdst!%(i,o)) v

x86_push (Constant n)                            = tellBC [Instruction x86_prefix (OpCode [0x68] Nothing) (Imm32 (fi n)) NoModRM]
x86_push (Variable (Register r))                 = tellBC [Instruction x86_prefix (OpCode [] (Just (0x50,toR r))) Imm0 NoModRM]
x86_push (Variable (AtOffset (Register r) i o))  = tellBC [Instruction x86_prefix (OpCode [0xff] Nothing) Imm0 (Disp8 (toEnum 6) (toRM r i) (archOffset o))]
x86_push (Variable (AtOffset l i o))             = do x86_cp (Register rsrc) (Variable l)
                                                      x86_push (Variable (rsrc!%(i,o)))
                                                   
x86_pop (Left n)                             = x86_add (Register (reg R_esp_ah)) (Constant (fromIntegral (x86_wordSize*n)))
x86_pop (Right (Register r))                 = tellBC [Instruction x86_prefix (OpCode [] (Just (0x58,toR r))) Imm0 NoModRM]
x86_pop (Right (AtOffset (Register r) i o))  = tellBC [Instruction x86_prefix (OpCode [0x8f] Nothing) Imm0 (Disp8 (toEnum 0) (toRM r i) (archOffset o))]
x86_pop (Right (AtOffset l i o))             = do x86_cp (Register rdst) (Variable l)
                                                  x86_pop (Right (rdst!%(i,o)))
                                    
x86_call (Constant i) = mdo
  if32
    (tellBC [Instruction zero (OpCode [0xe8] Nothing) (Imm32 (fi (fi i-b))) NoModRM])
    (do x86_cp (Register rsrc) (Constant i)
        tellBC [Instruction zero (OpCode [0xff] Nothing) Imm0 (Value (toEnum 2) (toR rsrc))])
  BA b <- getCounter
  return ()
x86_call (Variable (Register r))                 = tellBC [Instruction x86_prefix (OpCode [0xff] Nothing) Imm0 (Value (toEnum 2) (toR r))]
x86_call (Variable (AtOffset (Register r) i o))  = tellBC [Instruction x86_prefix (OpCode [0xff] Nothing) Imm0 (Disp8 (toEnum 2) (toRM r i) (archOffset o))]
x86_call (Variable (AtOffset l i o))             = do x86_cp (Register rsrc) (Variable l)
                                                      x86_call (Variable (rsrc!%(i,o)))

x86_ret = tellBC [Instruction zero (OpCode [0xc3] Nothing) Imm0 NoModRM]

reg = RegID . fromEnum


x86_cmp (Register a) (Constant b)
  = tellBC [Instruction x86_prefix (OpCode [0x81] Nothing) (Imm32 (fromIntegral b)) (Value (toEnum 7) (toR a))]
x86_cmp (Register a) (Variable (Register b))
  = tellBC [Instruction x86_prefix (OpCode [0x3b] Nothing) Imm0 (Value (toR a) (toR b))]
x86_cmp (Register a) (Variable (AtOffset (Register b) i o))
  = tellBC [Instruction x86_prefix (OpCode [0x3b] Nothing) Imm0 (Disp32 (toR a) (toRM b i) (archOffset o))]
x86_cmp (Register a) (Variable (AtOffset l i o))
  = do x86_cp (Register rsrc) (Variable l)
       x86_cmp (Register a) (Variable (rsrc!%(i,o)))
x86_cmp (AtOffset (Register a) i o) (Constant b)
  = tellBC [Instruction x86_prefix (OpCode [0x81] Nothing) (Imm32 (fromIntegral b)) (Disp32 (toEnum 7) (toRM a i) (archOffset o))]
x86_cmp (AtOffset (Register a) i o) (Variable (Register b))
  = tellBC [Instruction x86_prefix (OpCode [0x39] Nothing) Imm0 (Disp32 (toR b) (toRM a i) (archOffset o))]
x86_cmp (AtOffset (Register a) i o) (Variable l)
  = do x86_cp (Register rsrc) (Variable l)
       x86_cmp (a !% (i,o)) (Variable (Register rsrc))
x86_cmp (AtOffset l i o) v
  = do x86_cp (Register rdst) (Variable l)
       x86_cmp (rdst!%(i,o)) v

x86_jcmp _ (x,o) (Constant a) (Constant b) (BA addr) = censor $ do
  x86_jmp (Constant (fromIntegral addr))
  let doJmp = (compare a b == o) == x
  return ((),warp bData (\d -> if doJmp then d else zero) . warp bcSize (\s -> if doJmp then s else 0))
x86_jcmp h x (Variable a) b (BA addr) = void $ mfix $ \(BA here) -> do
  x86_cmp a b
  tellBC [Instruction (maybe id (\b -> touch (if b then BranchTaken else BranchNotTaken)) h zero) 
          (OpCode op Nothing) (Imm32 (fromIntegral (addr-here))) NoModRM]
  getCounter
  where op = case x of
          (True,LT) -> [0x0f,0x8c]
          (True,EQ) -> [0x0f,0x84]
          (True,GT) -> [0x0f,0x8f]
          (False,LT) -> [0x0f,0x8d]
          (False,EQ) -> [0x0f,0x85]
          (False,GT) -> [0x0f,0x8e]
x86_jcmp h x a b addr = x86_jcmp (map not h) (x&l'1 %~ not) b a addr

x86_jmp (Constant addr) = void $ mfix $ \(BA here) -> do
  tellBC [Instruction zero (OpCode [0xe9] Nothing) (Imm32 (fromIntegral (addr-fromIntegral here))) NoModRM]
  getCounter
x86_jmp (Variable (Register r)) = tellBC [Instruction x86_prefix (OpCode [0xff] Nothing) Imm0 (Value (toEnum 4) (toR r))]
x86_jmp v = do x86_cp (Register rsrc) v
               x86_jmp (Variable (Register rsrc))

x86_load (Register r) (BA a)
  = tellBC [Instruction x86_prefix (OpCode [0x8b] Nothing) (Imm32 (fromIntegral a))
            (NoDisp (toR r) (RM0_sib (SIB SC_1 I_none B_none)))]
x86_load l (BA a)
  = do x86_load (Register rsrc) (BA a)
       x86_cp l (Variable (Register rsrc))

x86_store (BA a) (Variable (Register r))
  = tellBC [Instruction x86_prefix (OpCode [0x89] Nothing) (Imm32 (fromIntegral a))
            (NoDisp (toR r) (RM0_sib (SIB SC_1 I_none B_none)))]
x86_store (BA a) x
  = do x86_cp (Register rsrc) x
       x86_store (BA a) (Variable (Register rsrc))

x86_ccall :: (?x86 :: X86, MonadASM m s) => Maybe Locus -> BinAddress -> [m Value] -> m ()
x86_ccall mret f args = let ?sys = x86_machine_common in do
  let hasReg r = case mret of
        Just l -> baseRegister l==reg r
        _ -> False
      pushArgs = for_ args $ \arg -> pushV =<< arg
      setDest r = maybe unit (<-- r) mret
      hasEax = hasReg R_eax
      saveReg = if hasReg R_ebx then R_ecx else R_ebx
      saved = if32 [R_ecx,R_edx] [R_ecx]
      doCall = if32 (call f) $ do
        traverse_ popV (reverse (zipWith const [R_edi_bh,R_esi_dh,R_edx,R_ecx,R_r8,R_r9] args))
        call f
  when hasEax $ do pushV saveReg
  pushing [R_eax] $ do
    pushing saved $ do
      pushArgs
      doCall
    if hasEax
      then saveReg <-- R_eax
      else setDest R_eax
  when hasEax $ do setDest saveReg; popV saveReg


x86_machine_common = VonNeumannMachine {
  _destReg = reg R_eax, _thisReg = reg R_ebx, _tmpReg = reg R_ecx,
  _newFunction = \sec -> case sec of
    TextSection -> align 16 0x90 >> getCounter
    DataSection -> align x86_wordSize 0 >> getCounter
    _ -> getCounter,
  _pushThunk = error "Undefined method pushThunk in basic X86 configuration",
  _popThunk = error "Undefined method popThunk in basic X86 configuration",
  _cp = x86_cp , _add = x86_add,
  _load = x86_load, _store = x86_store,
  _ret = x86_ret, _push = x86_push,
  _pop = x86_pop, _call = x86_call,
  _jcmp = x86_jcmp, _jmp = x86_jmp,
  _curlyBuiltin = let ?sys = x86_machine_common in liftA2 (+) commonBuiltin (assemblyBuiltin encodeWord),
  _assemblyMachine = Just AssemblyMachine {
    _ccall = x86_ccall,
    _poolReg = reg R_ebp_ch,
    _wordSize = x86_wordSize,
    _pageSize = 4096
    }
  }

x86_sys :: (?x86 :: X86)
           => String                                         -- ^ The system's name
           -> Standalone                                     -- ^ A wrapper for standalone programs
           -> ((?sysHooks :: SystemHooks) => VonNeumannMachine) -- ^ A machine description
           -> ((?sysHooks :: SystemHooks) => BUILTIN_INSTR)     -- ^ Additional per-system builtins
           -> SystemHooks                                       -- ^ Standalone hooks
           -> System
x86_sys name prog machine builtins hooks =
  System name (set (each.executePerm) True) prog (Just hooks) $
  Imperative (\mh -> case mh of
                 Just h -> let ?sysHooks = h in withNewCurlyBuiltins builtins machine
                 Nothing -> error "The X86 system must provide a JIT environment") 
