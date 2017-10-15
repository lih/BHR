{-# LANGUAGE CPP, TypeFamilies, ScopedTypeVariables, UndecidableInstances, StandaloneDeriving #-}
module Curly.System.Format.ELF where

import Language.Format
import Data.Bits

class (Serializable (Elf16 c),Serializable (Elf32 c),Serializable (Elf64 c),Serializable (ElfN c)) => ElfSystem c where
  data Elf16 c :: *
  data Elf32 c :: *
  data Elf64 c :: *
  data ElfN c :: *
  e_16 :: Word16 -> Elf16 c
  e_32 :: Word32 -> Elf32 c
  e_64 :: Word64 -> Elf64 c
  e_n :: Word32 -> ElfN c
  e_abi :: c -> (ElfMachine c,ElfABI,ElfBits,ElfOrder)

data Elf t = Elf {
  e_type :: ElfType t,
  e_entry :: ElfN t,
  e_programs :: [ElfSection t]
  }

data Linux_X86
linux_x86 :: Linux_X86
linux_x86 = linux_x86
instance ElfSystem Linux_X86 where
  newtype Elf16 Linux_X86 = Linux_X86_Elf16 Word16
  newtype Elf32 Linux_X86 = Linux_X86_Elf32 Word32
  newtype Elf64 Linux_X86 = Linux_X86_Elf64 Word64
  newtype ElfN Linux_X86 = Linux_X86_ElfN Word32
  e_16 = Linux_X86_Elf16
  e_32 = Linux_X86_Elf32
  e_64 = Linux_X86_Elf64
  e_n = Linux_X86_ElfN
  e_abi _ = (EM_X86,EABI_Linux,EB_32,EO_LSB)
instance Serializable (Elf16 Linux_X86) where
  encode (Linux_X86_Elf16 w) = encode (LittleEndian w)
instance Serializable (Elf32 Linux_X86) where
  encode (Linux_X86_Elf32 w) = encode (LittleEndian w)
instance Serializable (Elf64 Linux_X86) where
  encode (Linux_X86_Elf64 w) = encode (LittleEndian w)
instance Serializable (ElfN Linux_X86) where
  encode (Linux_X86_ElfN w) = encode (LittleEndian w)

data Linux_X86_64
linux_x64 :: Linux_X86_64
linux_x64 = linux_x64
instance ElfSystem Linux_X86_64 where
  newtype Elf16 Linux_X86_64 = Linux_X86_64_Elf16 Word16
  newtype Elf32 Linux_X86_64 = Linux_X86_64_Elf32 Word32
  newtype Elf64 Linux_X86_64 = Linux_X86_64_Elf64 Word64
  newtype ElfN Linux_X86_64 = Linux_X86_64_ElfN Word64
  e_16 = Linux_X86_64_Elf16
  e_32 = Linux_X86_64_Elf32
  e_64 = Linux_X86_64_Elf64
  e_n = Linux_X86_64_ElfN . fromIntegral
  e_abi _ = (EM_X86_64,EABI_Linux,EB_64,EO_LSB)
instance Serializable (Elf16 Linux_X86_64) where
  encode (Linux_X86_64_Elf16 w) = encode (LittleEndian w)
instance Serializable (Elf32 Linux_X86_64) where
  encode (Linux_X86_64_Elf32 w) = encode (LittleEndian w)
instance Serializable (Elf64 Linux_X86_64) where
  encode (Linux_X86_64_Elf64 w) = encode (LittleEndian w)
instance Serializable (ElfN Linux_X86_64) where
  encode (Linux_X86_64_ElfN w) = encode (LittleEndian w)

data Linux_ARM
linux_arm :: Linux_ARM
linux_arm = linux_arm
instance ElfSystem Linux_ARM where
  newtype Elf16 Linux_ARM = Linux_ARM_Elf16 Word16
  newtype Elf32 Linux_ARM = Linux_ARM_Elf32 Word32
  newtype Elf64 Linux_ARM = Linux_ARM_Elf64 Word64
  newtype ElfN Linux_ARM = Linux_ARM_ElfN Word32
  e_16 = Linux_ARM_Elf16
  e_32 = Linux_ARM_Elf32
  e_64 = Linux_ARM_Elf64
  e_n = Linux_ARM_ElfN
  e_abi _ = (EM_ARM,EABI_Linux,EB_32,EO_MSB)
instance Serializable (Elf16 Linux_ARM) where
  encode = coerceEncode Linux_ARM_Elf16 
instance Serializable (Elf32 Linux_ARM) where
  encode = coerceEncode Linux_ARM_Elf32 
instance Serializable (Elf64 Linux_ARM) where
  encode = coerceEncode Linux_ARM_Elf64 
instance Serializable (ElfN Linux_ARM) where
  encode = coerceEncode Linux_ARM_ElfN 

data ElfType t = ET_Exec
data ElfMachine t = EM_X86 | EM_X86_64 | EM_ARM
data ElfABI = EABI_Linux
data ElfBits = EB_32 | EB_64
data ElfOrder = EO_LSB | EO_MSB

data ElfSection t = ElfProgram {
  es_name :: String,
  es_vaddr :: ElfN t,
  es_flags :: (Bool,Bool,Bool),
  es_data :: Bytes
  }
                  | ElfSymTab {
  es_name :: String,
  es_symbols :: Map String (String,ElfN t)
  }

instance forall c. ElfSystem c => Serializable (ElfType c) where
  encode ET_Exec = encode (e_16 2 :: Elf16 c)
instance forall c. ElfSystem c => Serializable (ElfMachine c) where
  encode EM_X86 = encode (e_16 3 :: Elf16 c)
  encode EM_X86_64 = encode (e_16 62 :: Elf16 c)
  encode EM_ARM = encode (e_16 40 :: Elf16 c)
instance Serializable ElfABI where
  encode EABI_Linux = word8 3
instance Serializable ElfBits where
  encode EB_32 = word8 1
  encode EB_64 = word8 2
instance Serializable ElfOrder where
  encode EO_LSB = word8 1
  encode EO_MSB = word8 2

natSize :: (ElfSystem c,Num n) => c -> n
natSize c = case b of EB_32 -> 4 ; EB_64 -> 8
  where (_,_,b,_) = e_abi c
ehSize sys = natSize sys*3 + 40    :: Word16
phEntSize sys = 8 + 6*natSize sys  :: Word16
shEntSize sys = 16 + 6*natSize sys :: Word16
stEntSize sys = 8 + 2*natSize sys  :: Word16

sHN_UNDEF = 0
sHT_NULL = 0
sHT_PROGBITS = 1
sHT_SYMTAB = 2
sHT_STRTAB = 3
pT_LOAD = 1
sHF_WRITE = 1
sHF_ALLOC = 2
sHF_EXECINSTR = 4

instance forall c. ElfSystem c => Serializable (Elf c) where
  encode (Elf t entry progs) = ehdr + fold [by bytesBuilder dat | ElfProgram { es_data = dat } <- progs]
                               + fold pHdrs + fold sHdrs + by bytesBuilder strTable
                               + fold symtabs
    where vers :: Num n => n ; vers = 1
          enc16 n = encode (e_16 n :: Elf16 c)
          enc32 n = encode (e_32 n :: Elf32 c)
          encN n  = encode (e_n n :: ElfN c)
          sys = sys :: c
          
          phNum     = fromIntegral (length [() | ElfProgram _ _ _ _ <- progs]) :: Word16
          shNum     = fromIntegral (length progs) + 2                          :: Word16

          progOff = fromIntegral (ehSize sys)
          phOff = progOff + fromIntegral (fold [bytesSize dat | ElfProgram { es_data = dat } <- progs])
          shOff = phOff + fromIntegral (phEntSize sys) * fromIntegral phNum
          strTOff = shOff + fromIntegral (shEntSize sys) * fromIntegral shNum
          symTOff = strTOff + fromIntegral (bytesSize strTable)

          ehdr = ident + encode t + encode m + enc32 vers
                 + encode entry + encN phOff + encN (if shNum>0 then shOff else 0) + enc32 0
                 + enc16 (ehSize sys) + enc16 (phEntSize sys) + enc16 phNum + enc16 (shEntSize sys) + enc16 shNum
                 + enc16 1
                           
          hdrOffs = mapAccum_ withOff progs (progOff,symTOff)
            where withOff p@(ElfProgram _ _ _ dat) (offP,offS) = ((off',offS),(offP,p))
                    where off' = offP+fromIntegral (bytesSize dat)
                  withOff p@(ElfSymTab _ syms) (offP,offS) = ((offP,offS'),(offS,p))
                    where offS' = offS+fromIntegral (stEntSize sys * size syms)
          pHdrs = map pHdr hdrOffs
            where pHdr (off,ElfProgram _ vaddr (r,w,x) dat) = hdr
                    where hdr = case b of
                            EB_32 -> enc32 pT_LOAD + encN off + encode vaddr + encN 0
                                     + encN sz + encN sz
                                     + enc32 flags + encN align
                            EB_64 -> enc32 pT_LOAD + enc32 flags + encN off + encode vaddr + encN 0
                                     + encN sz + encN sz
                                     + encN align
                          flags = fromIntegral (fromEnum r`shiftL`2 + fromEnum w`shiftL`1 + fromEnum x)
                          align = 4096
                          sz = fromIntegral (bytesSize dat)
                  pHdr _ = zero

          sHdrs = zeroSHdr : strTHdr : map sHdr hdrOffs
            where sHdr (off,ElfProgram nm vaddr (_,w,x) dat) = hdr
                    where hdr = enc32 (strIndex nm) + enc32 sHT_PROGBITS + encN flags
                                + encode vaddr + encN off + encN sz
                                + enc32 sHN_UNDEF + enc32 0
                                + encN 0 + encN 0
                          flags = (if w then sHF_WRITE else 0) + sHF_ALLOC + (if x then sHF_EXECINSTR else 0)
                          sz = fromIntegral (bytesSize dat)
                  sHdr (off,ElfSymTab nm syms) =
                    enc32 (strIndex nm) + enc32 sHT_SYMTAB + encN 0
                    + encN 0 + encN off + encN sz
                    + enc32 1 + enc32 0
                    + encN 0 + encN (fromIntegral $ stEntSize sys)
                    where sz = fromIntegral (stEntSize sys * size syms)
                  strTHdr = enc32 0 + enc32 sHT_STRTAB + encN 0
                            + encN 0 + encN strTOff + encN (fromIntegral $ bytesSize strTable)
                            + enc32 sHN_UNDEF + enc32 0
                            + encN 0 + encN 0
                  zeroSHdr = enc32 0 + enc32 sHT_NULL + encN 0
                             + encN 0 + encN 0 + encN 0 + enc32 0 + enc32 0 + encN 0 + encN 0

          symbols = map es_name progs + [s | ElfSymTab _ syms <- progs, s <- keys syms]
          strIndex nm = fromIntegral (fromMaybe 0 (lookup nm nmInds))
            where nmInds = c'map $ fromAList $ mapAccum_ (\nm acc -> (acc+length nm+1,(nm,acc))) symbols 1
          strTable = stringBytes (foldMap ('\0':) symbols + "\0")

          symtabs = map stHdr hdrOffs
            where stHdr (_,ElfSymTab _ syms) = fold [sym n s v | (n,(s,v)) <- syms^.ascList]
                  stHdr _ = zero
                  sym n s v = let shndx = head [i | (s',i) <- zip (map es_name progs) [2..], s==s']
                              in case b of
                                EB_32 -> enc32 (strIndex n) + encode v + encN 0 + word8 0 + word8 0 + enc16 shndx
                                EB_64 -> enc32 (strIndex n) + word8 0 + word8 0 + enc16 shndx + encode v + encN 0
          ascii = fromIntegral . fromEnum
          (m,a,b,o) = e_abi sys
          ident = foldMap word8 [0x7f,ascii 'E',ascii 'L',ascii 'F']
                  + encode b + encode o + word8 vers + encode a
                  + foldMap word8 (0<$[8..15 :: Int])
