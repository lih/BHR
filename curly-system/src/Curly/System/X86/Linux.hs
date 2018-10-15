{-# LANGUAGE PatternSynonyms, RecursiveDo #-}
module Curly.System.X86.Linux where

import Definitive
import Language.Format
import Curly.Core
import Curly.System.Base
import Curly.System.X86.Common
import Curly.System.Format.ELF

x86_machine_linux :: (?x86 :: X86,?sysHooks :: SystemHooks) => VonNeumannMachine
x86_machine_linux = x86_machine_common {
  _pushThunk = _sysPushThunk ?sysHooks,
  _popThunk = _sysPopThunk ?sysHooks
  }

x86_common :: (?x86 :: X86) => ((?sys :: VonNeumannMachine) => a) -> a
x86_common a = let ?sys = x86_machine_common in a
x86_extended :: (?x86 :: X86,?sysHooks :: SystemHooks) => ((?sys :: VonNeumannMachine) => a) -> a
x86_extended a = let ?sys = x86_machine_linux in a

data X86_Syscall = SC_Brk
                 | SC_Write
                 | SC_Exit
                 | SC_Read
                 | SC_Open
                 | SC_Close

extraReg = reg R_edx
esp = reg R_esp_ah
r11 = reg R_r11

valReg :: (?sys :: VonNeumannMachine,MonadASM m s,IsValue v) => v -> m RegID
valReg v = case toValue v of
  Variable (Register r) -> return r
  v' -> do
    tmpReg <-- v'
    return tmpReg

x86_interrupt :: MonadASM m s => Word8 -> m ()
x86_interrupt n = tellBC [Instruction zero (OpCode [0xcd] Nothing) (Imm8 n) NoModRM]

argVal :: (MonadASM m s,IsValue v) => v -> m Value
argVal = return . toValue
x86_linux_syscall :: (?x86 :: X86, MonadASM m s) => X86_Syscall -> [m Value] -> m ()
x86_linux_syscall sc getArg = if32 syscall_32 syscall_64
  where syscall_32 = x86_common $
                     syscall_common [tmpReg,extraReg] [R_ebx,R_ecx,R_edx,R_esi_dh,R_edi_bh,R_ebp_ch]
                     (case sc of SC_Brk -> 0x2d; SC_Read -> 0x3; SC_Write -> 0x4
                                 SC_Exit -> 0x1; SC_Open -> 0x5; SC_Close -> 0x6)
                     (x86_interrupt 0x80)
        syscall_64 = x86_common $
                     syscall_common [tmpReg,r11] [R_edi_bh,R_esi_dh,R_edx,R_r10,R_r8,R_r9]
                     (case sc of SC_Brk -> 0xc; SC_Read -> 0x0; SC_Write -> 0x1
                                 SC_Exit -> 0x3c; SC_Open -> 0x2; SC_Close -> 0x3)
                     (tellBC [Instruction zero (OpCode [0x0f,0x05] Nothing) Imm0 NoModRM])
        syscall_common saved args n syscall = do
          pushing (saved + map snd (select (\(_,x) -> not (x`elem`(reg R_edi_bh:reg R_esi_dh:saved))) ar)) $ do
            sequence_ [pushV =<< arg | (arg,_) <- ar]
            sequence_ [popV r | (_,r) <- reverse ar]
            destReg <-- (n :: Int)
            syscall
           where ar = zipWith (,) getArg (map reg args)

x86_linux_malloc, x86_linux_memextend_, x86_linux_brkaddr :: (?x86 :: X86, MonadASM m s) => m BinAddress
x86_linux_memextend_page :: (?x86 :: X86,MonadASM m s) => m BinAddress

x86_linux_brkaddr = x86_common $ mfix $ \brk -> getOrDefine DataSection "brkaddr" $ do
  getOrDefine InitSection "brkaddr-init" $ do
    x86_linux_syscall SC_Brk [pure (Constant 0)]
    store brk destReg
  tell $ encodeWord 0
-- x86 brk: syscall 0x2d; ebx=<new break>
x86_linux_memextend_page = x86_common $ getOrDefine TextSection "memextend-page" $ do
  brk <- x86_linux_brkaddr
  pushing [destReg] $ do
    x86_linux_syscall SC_Brk [load tmpReg brk >> add tmpReg (pageSize :: Int) >> argVal tmpReg]
    store brk destReg
  ret
x86_linux_memextend_pool sz = x86_common $ getOrDefine TextSection ("memextend-pool-"+show sz) $ do
  ext <- x86_linux_memextend_page
  brk <- x86_linux_brkaddr
  call ext
  pushing [destReg,thisReg] $ do
    load thisReg brk
    destReg <-- thisReg
    add destReg (-pageSize :: Int)
    begin <- newFunction TextSection
    ifcmp (True,LT) destReg thisReg $ do
      destReg!Offset 0 <-- destReg
      add (destReg!Offset 0) (sz :: Int)
      add destReg (sz :: Int)
      jmp begin
  ret
x86_linux_memextend_ = x86_common $ getOrDefine TextSection "memextend_" $ do
  brk <- x86_linux_brkaddr
  tmpReg <-- esp!Offset (2*x86_wordSize)
  pushing [destReg] $ do
    x86_linux_syscall SC_Brk [argVal tmpReg]
    store brk destReg
  ret
x86_linux_malloc = x86_common $ getOrDefine TextSection "malloc" $ do
  let npools = 9
  pools <- getOrDefine DataSection "malloc-pools" $ reserve (npools*wordSize) 0
  let used = [thisReg,tmpReg]
  pushing used $ do
    destReg <-- esp ! Offset (x86_wordSize*(1+length used))
    brk <- x86_linux_brkaddr
    itecmp (True,GT) destReg (pageSize :: Int)
      (do extend <- x86_linux_memextend_
          load thisReg brk
          callWithStackArgs extend [argVal destReg]
          destReg <-- thisReg)
      (do let allocSize i sz = do
                let pool = pools + BA (i*wordSize)
                thisReg <-- pool
                ifcmp (True,EQ) (thisReg ! Offset 0) (0 :: Int) $ do
                  load thisReg brk
                  extend <- x86_linux_memextend_pool sz
                  call extend
                destReg <-- thisReg
                thisReg <-- thisReg!Offset 0
                store pool thisReg
          foldr (\(i,sz) x -> itecmp (False,GT) destReg (sz :: Int) (allocSize i sz) x) unit
            $ take npools $ iterate ((+1)<#>(*2)) (0,thunkSize))
  ret
x86_linux_sysAllocBytes d n = x86_common $ do
  alloc <- x86_linux_malloc
  case d of
    Register r | r==destReg -> callWithStackArgs alloc [argVal n]
    _ | baseRegister d==destReg -> pushing [extraReg] $ do pushing [destReg] $ do
                                                             callWithStackArgs alloc [argVal n]
                                                             extraReg <-- destReg
                                                           d <-- extraReg
    _ -> pushing [destReg] $ do callWithStackArgs alloc [argVal n]; d <-- destReg

x86_pushThunk, x86_popThunk :: (?x86 :: X86, MonadASM m s) => Locus -> m ()
x86_pushThunk dest = x86_common $ do
  ifcmp_hint (Just False) (True,EQ) poolReg (0 :: Integer) $ do
    load poolReg =<< x86_linux_brkaddr
    call =<< x86_linux_memextend_pool thunkSize
  poolReg ! EnvOffset <-- V dest
  dest <-- poolReg
  poolReg <-- poolReg ! Offset 0
x86_popThunk dest = x86_common $ do
  vr <- valReg $ V dest
  vr ! Offset 0 <-- poolReg
  poolReg <-- vr
  dest <-- vr ! EnvOffset

x86_linux_system :: (?x86 :: X86) => String -> System
x86_linux_system name = x86_sys name prog x86_machine_linux
                        (liftA2 (+) x86_linux_builtin (let ?sys = x86_machine_linux in assemblyBuiltin repr))
                        (SystemHooks x86_pushThunk x86_popThunk x86_linux_sysAllocBytes)
  where repr = SystemDataRepr (encode pBytes . LittleEndian) (encode pBytes . LittleEndian) (encode pBytes . LittleEndian)
               (if32 (encode pBytes . LittleEndian) (encode pBytes . LittleEndian . (fromIntegral :: Word32 -> Word64)))
               False (if32 4 8)
        pBytes :: Proxy Bytes; pBytes = Proxy
        prog = Standalone $ \mtext -> x86_common $ do
          mute $ rawProgram [InitSection,TextSection,DataSection] $ do
            rtSection InitSection .l'2 =- BA pstart
            rtSection TextSection .l'2 =~ \(BA x) -> BA (x+0x400000)
            rtSection DataSection .l'2 =~ \(BA x) -> (BA (x+0x400000))
            start <- mtext
            inSection InitSection $ do
              poolReg <-- Constant 0
              (dat,dat') <- inSection DataSection $ do
                align thunkSize 0
                liftA2 (,) (getCounter <* reserve thunkSize 0) (getCounter <* reserve thunkSize 0)
              destReg <-- dat
              thisReg <-- dat'
              call start
              x86_linux_syscall SC_Exit [pure (Constant 0)]
          rt <- get
          let BA tstart = BA 0x400000 + rt^.rtSection InitSection .l'2 
              BA dstart = BA 0x400000 + rt^.rtSection TextSection .l'2
              ser p = serialize $ Elf ET_Exec (p pstart) [
                ElfSection ".init" (ElfProgbits (p pstart) (True,False,True) (rt^.rtSection InitSection .l'1.bData)),
                ElfSection ".text" (ElfProgbits (p tstart) (True,False,True) (rt^.rtSection TextSection .l'1.bData)),
                ElfSection ".data" (ElfProgbits (p dstart) (True,True,False) (rt^.rtSection DataSection .l'1.bData)),
                ElfSection ".symtab" (ElfSymTab (from ascList $^ map (\((sec,sym),BA addr) -> (secName sec+"."+sym,(secName sec,p addr)))
                                                 $ ascList $^ rt^.rtBuiltins))]
              secName TextSection = ".text"
              secName InitSection = ".init"
              secName DataSection = ".data"
              secName (RawSection n) = ".raw."+n
          tell $ bytesCode' $ if32 (ser (Linux_X86_ElfN . fromIntegral)) (ser (Linux_X86_64_ElfN . fromIntegral))
        pstart = 0x400000 + fromIntegral (if32 (ehSize linux_x86) (ehSize linux_x64))

x86_defBuiltin :: (?x86 :: X86,?sysHooks :: SystemHooks,MonadASM m s) => String -> ((?sys :: VonNeumannMachine) => m ()) -> Maybe (m (BinAddress,Value))
x86_defBuiltin name m = Just $ x86_extended $ (,Constant 0) <$> getOrDefine TextSection name m

x86_linux_builtin :: (?x86 :: X86, ?sysHooks :: SystemHooks) => BUILTIN_INSTR
x86_linux_builtin B_Write = x86_defBuiltin "write" $ do
  [file,str,a] <- builtinArgs 3
  pushing [destReg] $ x86_linux_syscall SC_Write [
    do pushing [thisReg] $ callThunk file
       argVal (destReg!ValueOffset),
    do pushing [thisReg] $ callThunk str
       tmpReg <-- destReg!ValueOffset
       add tmpReg (2*x86_wordSize :: Int)
       argVal tmpReg,
    argVal (destReg!ValueOffset!Offset x86_wordSize)]
  tailCall a
x86_linux_builtin B_Read = x86_defBuiltin "read" $ do
  [file,count,cont] <- builtinArgs 3
  pushing [extraReg] $ do
    pushing [thisReg] $ callThunk count
    tmpReg <-- destReg!ValueOffset
    add tmpReg (2*wordSize :: Int)
    allocBytes extraReg tmpReg
    extraReg!Offset 0 <-- (1 :: Int)
    extraReg!Offset wordSize <-- destReg!ValueOffset
         
    pushing [destReg] $ x86_linux_syscall SC_Read [
      do pushing [thisReg] $ callThunk file
         argVal (destReg!ValueOffset),
    
      do tmpReg <-- extraReg
         add tmpReg (2*wordSize :: Int)
         argVal tmpReg,
        
      argVal (count!ValueOffset)
      ]

    pushing [thisReg] $ callThunk cont
    pushThunk (destReg!ValueOffset)
    tmpReg <-- destReg!ValueOffset
    cst <- global_constant
    setThunkVal tmpReg cst extraReg
  tailCall destReg
x86_linux_builtin B_Open = x86_defBuiltin "open" $ do
  [name,cont] <- builtinArgs 2
  pushing [extraReg] $ do
    pushing [thisReg] $ callThunk name
    pushing [destReg] $ do
      x86_linux_syscall SC_Open [tmpReg <-- destReg!ValueOffset
                                 >> add tmpReg (2*wordSize :: Int)
                                 >> argVal tmpReg]
      extraReg <-- destReg
    pushing [thisReg] $ callThunk cont
    pushThunk (destReg!ValueOffset)
    tmpReg <-- destReg!ValueOffset
    cst <- global_constant
    setThunkVal tmpReg cst extraReg
  tailCall destReg
x86_linux_builtin B_Close = x86_defBuiltin "close" $ do
  [file,a] <- builtinArgs 2
  pushing [thisReg] $ callThunk file
  pushing [destReg] $ x86_linux_syscall SC_Close [argVal (destReg!ValueOffset)]
  tailCall a
x86_linux_builtin B_StringLength = x86_defBuiltin "string-length" $ do
  [string] <- builtinArgs 1
  pushing [thisReg] $ callThunk string
  cst <- global_constant
  setThunkVal thisReg cst (destReg!ValueOffset!Offset wordSize)
  jmp cst
x86_linux_builtin _ = Nothing

system = let ?x86 = X86 in x86_linux_system "linux-x86"
system64 = let ?x86 = X86_64 in x86_linux_system "linux-x86-64"
