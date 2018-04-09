{-# Language RankNTypes, NoMonomorphismRestriction #-}
module Curly.System.ARM.Common where

import Definitive
import Language.Format
import Data.Bits
import Curly.System.Base
import IO.Filesystem

data R = R_r0 | R_r1 | R_r2 | R_r3 | R_r4 | R_r5 | R_r7
       | R_r8 | R_r9 | R_r10 | R_r11 | R_r12 | R_sp | R_lr | R_pc
       deriving (Enum,Show)
data Cond = C_EQ | C_NE | C_CS | C_CC
          | C_MI | C_PL | C_VS | C_VC
          | C_HI | C_LS | C_GE | C_LT
          | C_GT | C_LE | C_AL | C_None
          deriving (Enum,Show)
data Instruction = Conditional Cond Op
data Op = MOVL R Word16
        | MOVH R Word16
        | MOV R R
        | LDR R (R,Word16)
        | STR (R,Word16) R
        | CMPRR R R
        | CMPRI R Word16
        | Push [R]
        | Pop [R]
        | BLI Word32
        | BLR R
        | SVC Word32
        | Branch Word32 
instance IsLocus R where toLocus = Register . reg
instance IsValue R where toValue = Variable . toLocus

reg = RegID . fromEnum

mkInstr :: [(Int,Word32)] -> Word32
mkInstr l = foldl' (.|.) 0 [n `shiftL` sh | (sh,n) <- l]

archOffset :: (Semiring n,Num n) => Offset -> n
archOffset (Offset n) = fromIntegral n
archOffset EnvOffset = arm_wordSize
archOffset TypeOffset = 2*arm_wordSize
archOffset ValueOffset = 3*arm_wordSize

arm_wordSize = 4

en :: (Enum n,Num m) => n -> m
en = fromIntegral . fromEnum

instance BCSerializable Instruction where bcEncode = binaryCode (Just 4,4)
instance Serializable Instruction where
  encode (Conditional c op) = encode $ case op of
    MOVL r w -> mkInstr [(0,fromIntegral w .&. 0xfff),
                         (12,en r),
                         (16,fromIntegral w `shiftR` 12),
                         (20,0x30),
                         (28,en c)]
    MOVH r w -> mkInstr [(0,fromIntegral w .&. 0xfff),
                         (12,en r),
                         (16,fromIntegral w `shiftR` 12),
                         (20,0x34),
                         (28,en c)]
    MOV rd rs -> mkInstr [(0,en rs),
                          (12,en rd),
                          (16,0x1a0),
                          (28,en c)]
    LDR rd (rs,off) -> mkInstr [(0,fromIntegral off)
                               ,(12,en rd)
                               ,(16,en rs)
                               ,(20,0x51)
                               ,(23,en (off >= 0x800))
                               ,(28,en c)]
    STR (rd,off) rs -> mkInstr [(0,fromIntegral off)
                               ,(12,en rd)
                               ,(16,en rs)
                               ,(20,0x50)
                               ,(23,en (off >= 0x800))
                               ,(28,en c)]
    CMPRR a b -> mkInstr [(0,en a),(16,en b),(20,0x15),(28,en c)]
    CMPRI a n -> mkInstr [(0,fromIntegral n),(16,en a),(20,0x35),(28,en c)]
    Push rs -> mkInstr [(0,foldl' (.|.) 0 [1 `shiftL` fromEnum r | r <- rs])
                       ,(16,0x92d),(28,en c)]
    Pop rs -> mkInstr [(0,foldl' (.|.) 0 [1 `shiftL` fromEnum r | r <- rs])
                      ,(16,0x8bd),(28,en c)]
    BLI i -> mkInstr [(0,i`shiftR`2),(24,0xb),(28,en c)]
    BLR r -> mkInstr [(0,en r),(4,0x12fff3),(28,en c)]
    SVC s -> mkInstr [(0,s),(24,0xf),(28,en c)]
    Branch off -> mkInstr [(0,fromIntegral off),(24,0xa),(28,en c)]

toR (RegID r) = toEnum r

rsrc = reg R_r12
rdst = reg R_r11

arm_cp (Register r) (Constant c) = [Conditional C_AL (MOVL (toR r) (fromIntegral c))
                                           ,Conditional C_AL (MOVH (toR r) (fromIntegral (c`shiftR`16)))]
arm_cp (Register rd) (Variable (Register rs)) = [Conditional C_AL (MOV (toR rd) (toR rs))]
arm_cp (Register rd) (Variable (AtOffset (Register rs) _ off)) = [Conditional C_AL (LDR (toR rd) (toR rs,archOffset off))]
arm_cp (Register rd) (Variable (AtOffset l _ off)) = arm_cp (Register rsrc) (Variable l) + arm_cp (Register rd) (Variable (rsrc!off))
arm_cp (AtOffset (Register rd) _ off) (Variable (Register rs)) = [Conditional C_AL (STR (toR rd,archOffset off) (toR rs))]
arm_cp (AtOffset l _ off) v = arm_cp (Register rdst) (Variable l) + arm_cp (rdst!off) v

arm_add = undefined
arm_sub = undefined

arm_push (Variable (Register r)) = arm_instr [Conditional C_AL (Push [toR r])]
arm_push v = arm_instr $
             arm_cp (Register rsrc) v
             + [Conditional C_AL (Push [toR rsrc])]
arm_pop (Left n) = arm_add (Register (reg R_sp)) (Constant (fromIntegral ((n :: Int)*arm_wordSize)))
arm_pop (Right (Register r)) = arm_instr [Conditional C_AL (Pop [toR r])]
arm_pop (Right l) = arm_pop (Right (Register rsrc)) >> arm_instr (arm_cp l (Variable (Register rsrc)))
arm_ret :: MonadASM m s => m ()
arm_ret = arm_instr [Conditional C_AL (MOV R_pc R_lr)]
arm_call v = do
  arm_push (Variable (Register (reg R_lr)))
  BA here <- getCounter
  case v of
    Constant n -> arm_instr [Conditional C_AL (BLI (fromIntegral n - fromIntegral here - 8))]
    Variable (Register r) -> arm_instr [Conditional C_AL (BLR (toR r))]
    Variable _ -> arm_instr $ arm_cp (Register rsrc) v + [Conditional C_AL (BLR (toR rsrc))]
  arm_pop (Right (Register (reg R_lr)))

arm_jmp (Constant n) = do
  BA cur <- getCounter
  arm_instr [Conditional C_AL (Branch (fromIntegral n - fromIntegral cur - 8))]
arm_jmp (Variable (Register r)) = arm_instr [Conditional C_AL (MOV R_pc (toR r))]
arm_jmp (Variable (AtOffset (Register r) _ off)) = arm_instr [Conditional C_AL (LDR R_pc (toR r,archOffset off))]
arm_jmp (Variable (AtOffset l _ off)) = do
  arm_instr (arm_cp (Register rsrc) (Variable l))
  arm_jmp (Variable (rsrc!off))

arm_jcmp _ (inv,cmp) (Constant a) (Constant b) (BA addr) | (compare a b == cmp) == inv = arm_jmp (Constant (fromIntegral addr))
                                                         | otherwise = unit
arm_jcmp _ cmp (Variable a) (Variable b) (BA addr) = do
  ra <- toReg rsrc a
  rb <- toReg rdst b
  BA cur <- getCounter
  arm_instr [Conditional C_AL (CMPRR (toR ra) (toR rb))
            ,Conditional (cond cmp) (Branch (fromIntegral (addr - cur - 8)))]
arm_jcmp _ cmp (Variable a) (Constant b) (BA addr) = do
  ra <- toReg rsrc a
  BA cur <- getCounter
  arm_instr [Conditional C_AL (CMPRI (toR ra) (fromIntegral b))
            ,Conditional (cond cmp) (Branch (fromIntegral (addr - cur - 8)))]
arm_jcmp h cmp a b addr = arm_jcmp (map not h) (first not cmp) b a addr 

toReg _ (Register r) = return r
toReg def l = arm_instr (arm_cp (Register def) (Variable l)) >> return def

cond (True,LT) = C_LT
cond (True,EQ) = C_EQ
cond (True,GT) = C_GT
cond (False,LT) = C_GE
cond (False,EQ) = C_NE
cond (False,GT) = C_LE

arm_instr :: (MonadASM m s,Foldable f,BCSerializable s') => f s' -> m () 
arm_instr = tell . foldMap bcEncode

arm_curlyBuiltin x = Nothing
arm_load x = undefined
arm_store x = undefined

arm_machine = VonNeumannMachine {
  _destReg = reg R_r0, _thisReg = reg R_r1, _tmpReg = reg R_r2,
  _newFunction = \sec -> case sec of
    TextSection -> align 16 0x00 >> getCounter
    _ -> getCounter,
  _cp = cp, _add = arm_add, _sub = arm_sub,
  _pushThunk = \_ -> unit, _popThunk = \_ -> unit,
  _load = arm_load, _store = arm_store,
  _ret = arm_ret, _pop = arm_pop,
  _push = arm_push, _call = arm_call,
  _jmp = arm_jmp, _jcmp = arm_jcmp,
  _curlyBuiltin = arm_curlyBuiltin,
  _assemblyMachine = Nothing
  }
  where cp = map2 arm_instr arm_cp 

arm_sys :: String -> Standalone -> System
arm_sys name prog = System name (set (each.executePerm) True) prog Nothing $ Imperative (const arm_machine)
