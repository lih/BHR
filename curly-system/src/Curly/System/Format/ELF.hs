{-# LANGUAGE CPP, TypeFamilies, ScopedTypeVariables, UndecidableInstances, StandaloneDeriving, DeriveGeneric #-}
module Curly.System.Format.ELF where

import Language.Format
import Data.Bits

class (Format (Elf16 c),Format (Elf32 c),Format (Elf64 c),Format (ElfN c)) => ElfSystem c where
  data Elf16 c :: *
  data Elf32 c :: *
  data Elf64 c :: *
  data ElfN c :: *
  e_16 :: Word16 -> Elf16 c
  e_from16 :: Elf16 c -> Word16
  e_32 :: Word32 -> Elf32 c
  e_from32 :: Elf32 c -> Word32
  e_64 :: Word64 -> Elf64 c
  e_from64 :: Elf64 c -> Word64
  e_n :: Word32 -> ElfN c
  e_fromN :: ElfN c -> Word32
  e_abi :: c -> (ElfMachine c,ElfABI,ElfBits,ElfOrder)
class (Show (Elf16 c), Show (Elf32 c), Show (Elf64 c), Show (ElfN c)) => ElfShow c

instance ElfSystem c => Show (Elf16 c) where show = show . e_from16
instance ElfSystem c => Show (Elf32 c) where show = show . e_from32
instance ElfSystem c => Show (Elf64 c) where show = show . e_from64
 
data Linux_X86
linux_x86 :: Linux_X86
linux_x86 = linux_x86
instance ElfSystem Linux_X86 where
  newtype Elf16 Linux_X86 = Linux_X86_Elf16 Word16
  newtype Elf32 Linux_X86 = Linux_X86_Elf32 Word32
  newtype Elf64 Linux_X86 = Linux_X86_Elf64 Word64
  newtype ElfN Linux_X86 = Linux_X86_ElfN Word32
  e_16 = Linux_X86_Elf16
  e_from16 (Linux_X86_Elf16 x) = x
  e_32 = Linux_X86_Elf32
  e_from32 (Linux_X86_Elf32 x) = x
  e_64 = Linux_X86_Elf64
  e_from64 (Linux_X86_Elf64 x) = x
  e_n = Linux_X86_ElfN
  e_fromN (Linux_X86_ElfN x) = x
  e_abi _ = (EM_X86,EABI_Linux,EB_32,EO_LSB)
instance Serializable (Elf16 Linux_X86) where
  encode (Linux_X86_Elf16 w) = encode (LittleEndian w)
instance Format (Elf16 Linux_X86) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_Elf16 w
instance Serializable (Elf32 Linux_X86) where
  encode (Linux_X86_Elf32 w) = encode (LittleEndian w)
instance Format (Elf32 Linux_X86) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_Elf32 w
instance Serializable (Elf64 Linux_X86) where
  encode (Linux_X86_Elf64 w) = encode (LittleEndian w)
instance Format (Elf64 Linux_X86) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_Elf64 w
instance Serializable (ElfN Linux_X86) where
  encode (Linux_X86_ElfN w) = encode (LittleEndian w)
instance Format (ElfN Linux_X86) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_ElfN w
instance Show (ElfN Linux_X86) where
  show (Linux_X86_ElfN x) = show x
instance ElfShow Linux_X86

data Linux_X86_64
linux_x64 :: Linux_X86_64
linux_x64 = linux_x64
instance ElfSystem Linux_X86_64 where
  newtype Elf16 Linux_X86_64 = Linux_X86_64_Elf16 Word16
  newtype Elf32 Linux_X86_64 = Linux_X86_64_Elf32 Word32
  newtype Elf64 Linux_X86_64 = Linux_X86_64_Elf64 Word64
  newtype ElfN Linux_X86_64 = Linux_X86_64_ElfN Word64
  e_16 = Linux_X86_64_Elf16
  e_from16 (Linux_X86_64_Elf16 x) = x
  e_32 = Linux_X86_64_Elf32
  e_from32 (Linux_X86_64_Elf32 x) = x
  e_64 = Linux_X86_64_Elf64
  e_from64 (Linux_X86_64_Elf64 x) = x
  e_n = Linux_X86_64_ElfN . fromIntegral
  e_fromN (Linux_X86_64_ElfN x) = fromIntegral x
  e_abi _ = (EM_X86_64,EABI_Linux,EB_64,EO_LSB)
instance Serializable (Elf16 Linux_X86_64) where
  encode (Linux_X86_64_Elf16 w) = encode (LittleEndian w)
instance Format (Elf16 Linux_X86_64) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_64_Elf16 w
instance Serializable (Elf32 Linux_X86_64) where
  encode (Linux_X86_64_Elf32 w) = encode (LittleEndian w)
instance Format (Elf32 Linux_X86_64) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_64_Elf32 w
instance Serializable (Elf64 Linux_X86_64) where
  encode (Linux_X86_64_Elf64 w) = encode (LittleEndian w)
instance Format (Elf64 Linux_X86_64) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_64_Elf64 w
instance Serializable (ElfN Linux_X86_64) where
  encode (Linux_X86_64_ElfN w) = encode (LittleEndian w)
instance Format (ElfN Linux_X86_64) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_64_ElfN w
instance Show (ElfN Linux_X86_64) where
  show (Linux_X86_64_ElfN x) = show x
instance ElfShow Linux_X86_64
 
data Linux_ARM
linux_arm :: Linux_ARM
linux_arm = linux_arm
instance ElfSystem Linux_ARM where
  newtype Elf16 Linux_ARM = Linux_ARM_Elf16 Word16
  newtype Elf32 Linux_ARM = Linux_ARM_Elf32 Word32
  newtype Elf64 Linux_ARM = Linux_ARM_Elf64 Word64
  newtype ElfN Linux_ARM = Linux_ARM_ElfN Word32
  e_16 = Linux_ARM_Elf16
  e_from16 (Linux_ARM_Elf16 x) = x
  e_32 = Linux_ARM_Elf32
  e_from32 (Linux_ARM_Elf32 x) = x
  e_64 = Linux_ARM_Elf64
  e_from64 (Linux_ARM_Elf64 x) = x
  e_n = Linux_ARM_ElfN
  e_fromN (Linux_ARM_ElfN x) = x
  e_abi _ = (EM_ARM,EABI_Linux,EB_32,EO_MSB)
instance Serializable (Elf16 Linux_ARM) where
  encode (Linux_ARM_Elf16 w) = encode w
instance Format (Elf16 Linux_ARM) where
  datum = datum <&> \w -> Linux_ARM_Elf16 w
instance Serializable (Elf32 Linux_ARM) where
  encode (Linux_ARM_Elf32 w) = encode w
instance Format (Elf32 Linux_ARM) where
  datum = datum <&> \w -> Linux_ARM_Elf32 w
instance Serializable (Elf64 Linux_ARM) where
  encode (Linux_ARM_Elf64 w) = encode w
instance Format (Elf64 Linux_ARM) where
  datum = datum <&> \w -> Linux_ARM_Elf64 w
instance Serializable (ElfN Linux_ARM) where
  encode (Linux_ARM_ElfN w) = encode w
instance Format (ElfN Linux_ARM) where
  datum = datum <&> \w -> Linux_ARM_ElfN w
instance Show (ElfN Linux_ARM) where
  show (Linux_ARM_ElfN x) = show x
instance ElfShow Linux_ARM
 
data Elf t = Elf {
  e_type :: ElfType t,
  e_entry :: ElfN t,
  e_programs :: [ElfSection t]
  }

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

deriving instance ElfShow c => Show (ElfSection c)
deriving instance ElfShow c => Show (Elf c)

enumEncode :: (Enum a, Bounded a, Serializable s) => (t -> s) -> (a -> t) -> a -> Builder
enumEncode enc enum x = encode (enc $ enum x)
enumDatum :: (Eq t, Enum a, Bounded a, Format s) => (s -> t) -> (a -> t) -> Parser Bytes a
enumDatum dec enum = datum >>= \x -> case lookup (dec x) e of
  Just a -> return a
  _ -> noParse
  where e = [(enum y,y) | y <- [minBound..maxBound]]
c'word8 :: Constraint Word8
c'word8 = c'_

data ElfABI = EABI_Linux
            | EABI_SysV
            deriving (Bounded,Enum,Show)
enum'ElfABI EABI_SysV = 0
enum'ElfABI EABI_Linux = 3
instance Serializable ElfABI where encode = enumEncode c'word8 enum'ElfABI
instance Format ElfABI       where datum = enumDatum c'word8 enum'ElfABI

data ElfBits = EB_32 | EB_64
             deriving (Bounded,Enum,Show)
enum'ElfBits EB_32 = 1
enum'ElfBits EB_64 = 2
instance Serializable ElfBits where encode = enumEncode c'word8 enum'ElfBits
instance Format ElfBits       where datum = enumDatum c'word8 enum'ElfBits

data ElfOrder = EO_LSB | EO_MSB
              deriving (Bounded,Enum,Show)
enum'ElfOrder EO_LSB = 1
enum'ElfOrder EO_MSB = 2
instance Serializable ElfOrder where
  encode = enumEncode c'word8 enum'ElfOrder
instance Format ElfOrder where
  datum = enumDatum c'word8 enum'ElfOrder
 
data ElfType t = ET_Exec
               | ET_Rel
               deriving (Bounded,Enum,Show)
enum'ElfType ET_Exec = 2
enum'ElfType ET_Rel = 1
instance forall c. ElfSystem c => Serializable (ElfType c) where
  encode = enumEncode (e_16 :: Word16 -> Elf16 c) enum'ElfType
instance ElfSystem c => Format (ElfType c) where
  datum = enumDatum (e_from16 :: Elf16 c -> Word16) enum'ElfType
 
data ElfMachine t = EM_X86 | EM_X86_64 | EM_ARM
                  deriving (Bounded,Enum,Show)
enum'ElfMachine EM_X86 = 3
enum'ElfMachine EM_X86_64 = 62
enum'ElfMachine EM_ARM = 40
instance forall c. ElfSystem c => Serializable (ElfMachine c) where
  encode = enumEncode (e_16 :: Word16 -> Elf16 c) enum'ElfMachine 
instance ElfSystem c => Format (ElfMachine c) where
  datum = enumDatum (e_from16 :: Elf16 c -> Word16) enum'ElfMachine
 
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

eH_MAGIC = 0x7f454c46

data ElfIdent = ElfIdent {
  ei_magic :: Word32,
  ei_bits :: ElfBits,
  ei_order :: ElfOrder,
  ei_elfVersion :: Word8,  -- equals 1 
  ei_abi :: ElfABI,         
  ei_abiVersion :: Word8,  -- equals 0
  ei_identPad :: (Word8,Word16,Word32) -- all null
  }
              deriving (Generic,Show)
instance Serializable ElfIdent
instance Format ElfIdent
 
data ElfHeader t = ElfHeader {
  -- ident
  eh_ident :: ElfIdent,
  eh_type :: ElfType t,
  eh_machine :: ElfMachine t,
  eh_fileVersion :: Elf32 t, -- equals 1
  eh_entry, eh_phoff, eh_shoff :: ElfN t,
  eh_flags :: Elf32 t,
  eh_ehsize, eh_phentsize, eh_phnum, eh_shentsize, eh_shnum, eh_shstrndx :: Elf16 t
  }
                 deriving Generic
deriving instance ElfShow c => Show (ElfHeader c)
instance ElfSystem sys => Serializable (ElfHeader sys)
instance ElfSystem sys => Format (ElfHeader sys)

data ElfPHType t = PHT_Load
                 deriving (Enum,Bounded,Show)
enum'ElfPHType PHT_Load = 1
instance ElfSystem sys => Serializable (ElfPHType sys) where
  encode = enumEncode (e_32 :: Word32 -> Elf32 sys) enum'ElfPHType
instance ElfSystem sys => Format (ElfPHType sys) where
  datum = enumDatum (e_from32 :: Elf32 sys -> Word32) enum'ElfPHType
 
data ElfProgramHeader sys = ElfProgramHeader {
  eph_type :: ElfPHType sys,
  eph_flags :: Elf32 sys,
  eph_offset, eph_vaddr, eph_paddr,
  eph_filesz, eph_memsz, eph_align :: ElfN sys
  }
deriving instance ElfShow c => Show (ElfProgramHeader c)
instance ElfSystem sys => Serializable (ElfProgramHeader sys) where
  encode (ElfProgramHeader t f o va pa fs ms al) = case bits of
    EB_32 -> encode (t,o,va,pa,fs,ms,f,al)
    EB_64 -> encode (t,f,o,va,pa,fs,ms,al)
    where (_,_,bits,_) = e_abi (undefined :: sys)
instance ElfSystem sys => Format (ElfProgramHeader sys) where
  datum = case bits of
    EB_32 -> datum <&> \(t,o,va,pa,fs,ms,f,al) -> ElfProgramHeader t f o va pa fs ms al
    EB_64 -> datum <&> \(t,f,o,va,pa,fs,ms,al) -> ElfProgramHeader t f o va pa fs ms al
    where (_,_,bits,_) = e_abi (undefined :: sys)
 
data ElfSHType t = SHT_SymTab
                 | SHT_ProgBits
                 | SHT_NoBits
                 | SHT_StrTab
                 | SHT_Rel
                 | SHT_Rela
                 | SHT_Hash
                 | SHT_Dynamic
                 | SHT_Null
               deriving (Bounded,Enum,Show)
enum'ElfSHType SHT_Null = 0
enum'ElfSHType SHT_ProgBits = 1
enum'ElfSHType SHT_SymTab = 2
enum'ElfSHType SHT_StrTab = 3
enum'ElfSHType SHT_Rela = 4
enum'ElfSHType SHT_Hash = 5
enum'ElfSHType SHT_Dynamic = 6
enum'ElfSHType SHT_Rel = 9
enum'ElfSHType SHT_NoBits = 8
instance ElfSystem sys => Serializable (ElfSHType sys) where
  encode = enumEncode (e_32 :: Word32 -> Elf32 sys) enum'ElfSHType
instance ElfSystem sys => Format (ElfSHType sys) where
  datum = enumDatum (e_from32 :: Elf32 sys -> Word32) enum'ElfSHType
 
data ElfSectionHeader t = ElfSectionHeader {
  esh_name :: Elf32 t,
  esh_type :: ElfSHType t,
  esh_flags :: ElfN t,
  esh_addr, esh_offset, esh_size :: ElfN t,
  esh_link, esh_info :: Elf32 t,
  esh_addralign, esh_entsize :: ElfN t
  }
                        deriving Generic
deriving instance ElfShow c => Show (ElfSectionHeader c)
instance ElfSystem sys => Serializable (ElfSectionHeader sys)
instance ElfSystem sys => Format (ElfSectionHeader sys)
nullSectionHeader :: ElfSystem sys => ElfSectionHeader sys
nullSectionHeader = ElfSectionHeader (e_32 0) SHT_Null (e_n 0) (e_n 0) (e_n 0) (e_n 0) (e_32 0) (e_32 0) (e_n 0) (e_n 0)

data ElfSym t = ElfSym {
  esy_name :: Elf32 t,
  esy_info, esy_other :: Word8,
  esy_shndx :: Elf16 t,
  esy_value, esy_size :: ElfN t
  }
              deriving Generic
deriving instance ElfShow c => Show (ElfSym c)
instance ElfSystem sys => Serializable (ElfSym sys) where
  encode (ElfSym n i o ndx v sz) = case bits of
    EB_32 -> encode (n,v,sz,i,o,ndx)
    EB_64 -> encode (n,i,o,ndx,v,sz)
    where (_,_,bits,_) = e_abi (undefined :: sys)
instance ElfSystem sys => Format (ElfSym sys) where
  datum = case bits of
    EB_32 -> datum <&> \(n,v,sz,i,o,ndx) -> ElfSym n i o ndx v sz
    EB_64 -> datum <&> \(n,i,o,ndx,v,sz) -> ElfSym n i o ndx v sz
    where (_,_,bits,_) = e_abi (undefined :: sys)
nullSym :: ElfSystem sys => ElfSym sys
nullSym = ElfSym (e_32 0) 0 0 (e_16 0) (e_n 0) (e_n 0)

data ElfRelInfo t = ElfRelInfo {
  eri_sym :: Elf32 t,
  eri_type :: Word8
  }
                  deriving Show
instance ElfSystem sys => Serializable (ElfRelInfo sys) where
  encode (ElfRelInfo s t) = case bits of
    EB_32 -> encode (e_32 $ (e_from32 s`shiftL`8) .&. fromIntegral t :: Elf32 sys)
    EB_64 -> encode (e_64 $ (fromIntegral (e_from32 s)`shiftL`32) .&. fromIntegral t :: Elf64 sys)
    where (_,_,bits,_) = e_abi (undefined :: sys)
instance ElfSystem sys => Format (ElfRelInfo sys) where
  datum = case bits of
    EB_32 -> datum <&> \x -> ElfRelInfo (e_32 (e_from32 (x :: Elf32 sys)`shiftR`8)) (fromIntegral (e_from32 x))
    EB_64 -> datum <&> \x -> ElfRelInfo (e_32 (fromIntegral (e_from64 (x :: Elf64 sys)`shiftR`32))) (fromIntegral (e_from64 x))
    where (_,_,bits,_) = e_abi (undefined :: sys)
data ElfRel t = ElfRel {
  er_offset :: ElfN t,
  er_info :: ElfRelInfo t
  }
              deriving Generic
instance ElfSystem sys => Serializable (ElfRel sys)
instance ElfSystem sys => Format (ElfRel sys)

instance forall c. ElfSystem c => Serializable (Elf c) where
  encode (Elf t entry progs) = ehdr + fold [by bytesBuilder dat | ElfProgram { es_data = dat } <- progs]
                               + fold pHdrs + fold sHdrs + by bytesBuilder strTable
                               + fold symtabs
    where vers :: Num n => n ; vers = 1
          enc16 n = encode (e_16 n :: Elf16 c)
          enc32 n = encode (e_32 n :: Elf32 c)
          encN n  = encode (e_n n :: ElfN c)
          sys = sys :: c
          (m,a,b,o) = e_abi sys
          
          phNum     = fromIntegral (length [() | ElfProgram _ _ _ _ <- progs]) :: Word16
          shNum     = fromIntegral (length progs) + 2                          :: Word16

          progOff = fromIntegral (ehSize sys)
          phOff = progOff + fromIntegral (fold [bytesSize dat | ElfProgram { es_data = dat } <- progs])
          shOff = phOff + fromIntegral (phEntSize sys) * fromIntegral phNum
          strTOff = shOff + fromIntegral (shEntSize sys) * fromIntegral shNum
          symTOff = strTOff + fromIntegral (bytesSize strTable)

          ehdr = encode (ElfHeader
                         (ElfIdent eH_MAGIC b o 1 a 0 (0,0,0))
                         t m (e_32 1)
                         entry (e_n phOff) (e_n shOff)
                         (e_32 0)
                         (e_16 $ ehSize sys)
                         (e_16 $ phEntSize sys) (e_16 phNum)
                         (e_16 $ shEntSize sys) (e_16 shNum)
                         (e_16 1))
                           
          hdrOffs = mapAccum_ withOff progs (progOff,symTOff)
            where withOff p@(ElfProgram _ _ _ dat) (offP,offS) = ((off',offS),(offP,p))
                    where off' = offP+fromIntegral (bytesSize dat)
                  withOff p@(ElfSymTab _ syms) (offP,offS) = ((offP,offS'),(offS,p))
                    where offS' = offS+fromIntegral (stEntSize sys * size syms)
          pHdrs = map pHdr hdrOffs
            where pHdr (off,ElfProgram _ vaddr (r,w,x) dat) = encode ElfProgramHeader {
                    eph_type = PHT_Load,
                    eph_flags = e_32 flags,
                    eph_offset = e_n off, eph_vaddr = vaddr, eph_paddr = e_n 0,
                    eph_filesz = e_n sz, eph_memsz = e_n sz,
                    eph_align = e_n 4096
                    }
                    where flags = fromIntegral (fromEnum r`shiftL`2 + fromEnum w`shiftL`1 + fromEnum x)
                          sz = fromIntegral (bytesSize dat)
                  pHdr _ = zero

          sHdrs = map encode (zeroSHdr : strTHdr : map sHdr hdrOffs :: [ElfSectionHeader c])
            where sHdr (off,ElfProgram nm vaddr (_,w,x) dat) = nullSectionHeader {
                    esh_name = e_32 (strIndex nm),
                    esh_type = SHT_ProgBits,
                    esh_flags = e_n flags,
                    esh_addr = vaddr, esh_offset = e_n off, esh_size = e_n sz
                    }
                    where flags = (if w then sHF_WRITE else 0) + sHF_ALLOC + (if x then sHF_EXECINSTR else 0)
                          sz = fromIntegral (bytesSize dat)
                  sHdr (off,ElfSymTab nm syms) = nullSectionHeader {
                    esh_name = e_32 (strIndex nm),
                    esh_type = SHT_SymTab,
                    esh_offset = e_n off, esh_size = e_n sz,
                    esh_entsize = e_n (fromIntegral $ stEntSize sys),
                    esh_link = e_32 1, esh_info = e_32 (size syms)
                    }
                    where sz = fromIntegral (stEntSize sys * size syms)
                  strTHdr = nullSectionHeader {
                    esh_type = SHT_StrTab,
                    esh_offset = e_n strTOff, esh_size = e_n (fromIntegral $ bytesSize strTable)
                    }
                  zeroSHdr = nullSectionHeader

          symbols = map es_name progs + [s | ElfSymTab _ syms <- progs, s <- keys syms]
          strIndex nm = fromIntegral (fromMaybe 0 (lookup nm nmInds))
            where nmInds = c'map $ fromAList $ mapAccum_ (\nm acc -> (acc+length nm+1,(nm,acc))) symbols 1
          strTable = stringBytes (foldMap ('\0':) symbols + "\0")

          symtabs = map stHdr hdrOffs
            where stHdr (_,ElfSymTab _ syms) = fold [sym n s v | (n,(s,v)) <- syms^.ascList]
                  stHdr _ = zero
                  sym n s v = let shndx = head [i | (s',i) <- zip (map es_name progs) [2..], s==s']
                              in encode nullSym { esy_name = e_32 (strIndex n), esy_value = v, esy_shndx = e_16 shndx }

instance forall c. (ElfShow c, ElfSystem c) => Format (Elf c) where
  datum = do
    file <- remaining
    elfH <- datum
    let atOffset :: Integral n => n -> Parser Bytes a -> Parser Bytes a
        atOffset off m = do
          old <- runStreamState (id <~ \x -> (drop (fromIntegral off) file,x))
          m <* runStreamState (put old)
        readNRecords :: forall a n. (Enum n, Num n) => n -> Parser Bytes a -> Parser Bytes [a]
        readNRecords n m = for [1..n] $ \_ -> m
    trace (show elfH) unit
    secHdrs <- atOffset (e_fromN (eh_shoff elfH)) $ do
      readNRecords (e_from16 (eh_shnum elfH)) datum
    progHdrs <- atOffset (e_fromN (eh_phoff elfH)) $ do
      readNRecords (e_from16 (eh_phnum elfH)) datum
    let x = secHdrs :: [ElfSectionHeader c]; y = progHdrs :: [ElfProgramHeader c]
    trace (intercalate "\n" $ map show secHdrs + map show progHdrs) $ return ()
    return (Elf (eh_type elfH) (eh_entry elfH) [])
