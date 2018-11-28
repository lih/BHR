{-# LANGUAGE CPP, TypeFamilies, ScopedTypeVariables, UndecidableInstances, StandaloneDeriving, DeriveGeneric, DefaultSignatures, NamedFieldPuns, LambdaCase #-}
module Curly.System.Format.ELF where

import qualified Prelude as P
import Language.Format
import Data.Bits

class (Format Bytes (Elf16 sys),Format Bytes (Elf32 sys),Format Bytes (Elf64 sys),Format Bytes (ElfN sys)) => ElfSystem sys where
  data Elf16 sys :: *
  data Elf32 sys :: *
  data Elf64 sys :: *
  data ElfN sys :: *
  e_16 :: Word16 -> Elf16 sys
  e_from16 :: Elf16 sys -> Word16
  e_32 :: Word32 -> Elf32 sys
  e_from32 :: Elf32 sys -> Word32
  e_64 :: Word64 -> Elf64 sys
  e_from64 :: Elf64 sys -> Word64
  e_n :: Word32 -> ElfN sys
  e_fromN :: ElfN sys -> Word32
  e_abi :: sys -> (ElfMachine sys,ElfABI,ElfBits,ElfOrder)
class (Show (Elf16 sys), Show (Elf32 sys), Show (Elf64 sys), Show (ElfN sys)) => ElfShow sys

instance ElfSystem sys => Show (Elf16 sys) where show = show . e_from16
instance ElfSystem sys => Show (Elf32 sys) where show = show . e_from32
instance ElfSystem sys => Show (Elf64 sys) where show = show . e_from64
 
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
instance Serializable Bytes (Elf16 Linux_X86) where
  encode p (Linux_X86_Elf16 w) = encode p (LittleEndian w)
instance Format Bytes (Elf16 Linux_X86) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_Elf16 w
instance Serializable Bytes (Elf32 Linux_X86) where
  encode p (Linux_X86_Elf32 w) = encode p (LittleEndian w)
instance Format Bytes (Elf32 Linux_X86) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_Elf32 w
instance Serializable Bytes (Elf64 Linux_X86) where
  encode p (Linux_X86_Elf64 w) = encode p (LittleEndian w)
instance Format Bytes (Elf64 Linux_X86) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_Elf64 w
instance Serializable Bytes (ElfN Linux_X86) where
  encode p (Linux_X86_ElfN w) = encode p (LittleEndian w)
instance Format Bytes (ElfN Linux_X86) where
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
instance Serializable Bytes (Elf16 Linux_X86_64) where
  encode p (Linux_X86_64_Elf16 w) = encode p (LittleEndian w)
instance Format Bytes (Elf16 Linux_X86_64) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_64_Elf16 w
instance Serializable Bytes (Elf32 Linux_X86_64) where
  encode p (Linux_X86_64_Elf32 w) = encode p (LittleEndian w)
instance Format Bytes (Elf32 Linux_X86_64) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_64_Elf32 w
instance Serializable Bytes (Elf64 Linux_X86_64) where
  encode p (Linux_X86_64_Elf64 w) = encode p (LittleEndian w)
instance Format Bytes (Elf64 Linux_X86_64) where
  datum = datum <&> \(LittleEndian w) -> Linux_X86_64_Elf64 w
instance Serializable Bytes (ElfN Linux_X86_64) where
  encode p (Linux_X86_64_ElfN w) = encode p (LittleEndian w)
instance Format Bytes (ElfN Linux_X86_64) where
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
instance Serializable Bytes (Elf16 Linux_ARM) where
  encode p (Linux_ARM_Elf16 w) = encode p w
instance Format Bytes (Elf16 Linux_ARM) where
  datum = datum <&> \w -> Linux_ARM_Elf16 w
instance Serializable Bytes (Elf32 Linux_ARM) where
  encode p (Linux_ARM_Elf32 w) = encode p w
instance Format Bytes (Elf32 Linux_ARM) where
  datum = datum <&> \w -> Linux_ARM_Elf32 w
instance Serializable Bytes (Elf64 Linux_ARM) where
  encode p (Linux_ARM_Elf64 w) = encode p w
instance Format Bytes (Elf64 Linux_ARM) where
  datum = datum <&> \w -> Linux_ARM_Elf64 w
instance Serializable Bytes (ElfN Linux_ARM) where
  encode p (Linux_ARM_ElfN w) = encode p w
instance Format Bytes (ElfN Linux_ARM) where
  datum = datum <&> \w -> Linux_ARM_ElfN w
instance Show (ElfN Linux_ARM) where
  show (Linux_ARM_ElfN x) = show x
instance ElfShow Linux_ARM

instance ElfSystem sys => Eq (Elf16 sys) where a == b = e_from16 a == e_from16 b
instance ElfSystem sys => Ord (Elf16 sys) where compare = comparing e_from16
instance ElfSystem sys => Eq (Elf32 sys) where a == b = e_from32 a == e_from32 b
instance ElfSystem sys => Ord (Elf32 sys) where compare = comparing e_from32
instance ElfSystem sys => Eq (Elf64 sys) where a == b = e_from64 a == e_from64 b
instance ElfSystem sys => Ord (Elf64 sys) where compare = comparing e_from64
instance ElfSystem sys => Eq (ElfN sys) where a == b = e_fromN a == e_fromN b
instance ElfSystem sys => Ord (ElfN sys) where compare = comparing e_fromN

data ElfFlags t = ElfFlags (Set t)
                deriving Show
fromFlagList l = ElfFlags (fromKList [x | (b,x) <- l, b])
isFlagSet f (ElfFlags fs) = f`isKeyIn`fs
class HasFlagOps t where
  nullFlag :: t
  addFlags :: t -> t -> t
  hasFlags :: t -> t -> Bool
instance HasFlagOps Word8 where
  nullFlag = 0
  addFlags = (.|.)
  hasFlags x y = (x .&. y) == y
instance ElfSystem sys => HasFlagOps (Elf16 sys) where
  nullFlag = e_16 0
  addFlags x y = e_16 (e_from16 x .|. e_from16 y)
  hasFlags x y = (e_from16 x .&. e_from16 y) == e_from16 y
instance ElfSystem sys => HasFlagOps (Elf32 sys) where
  nullFlag = e_32 0
  addFlags x y = e_32 (e_from32 x .|. e_from32 y)
  hasFlags x y = (e_from32 x .&. e_from32 y) == e_from32 y
instance ElfSystem sys => HasFlagOps (Elf64 sys) where
  nullFlag = e_64 0
  addFlags x y = e_64 (e_from64 x .|. e_from64 y)
  hasFlags x y = (e_from64 x .&. e_from64 y) == e_from64 y
instance ElfSystem sys => HasFlagOps (ElfN sys) where
  nullFlag = e_n 0
  addFlags x y = e_n (e_fromN x .|. e_fromN y)
  hasFlags x y = (e_fromN x .&. e_fromN y) == e_fromN y
instance (ElfEnum flags u,HasFlagOps u,Serializable Bytes u) => Serializable Bytes (ElfFlags flags) where
  encode p (ElfFlags fs) = encode p (foldr addFlags nullFlag (map fromElfEnum fs))
instance (ElfEnum flags u,Ord flags,HasFlagOps u,Format Bytes u) => Format Bytes (ElfFlags flags) where
  datum = datum <&> \x -> ElfFlags $ compose [touch f | f <- [minBound..maxBound], hasFlags x (fromElfEnum f)] zero

toElfEnum_default :: (Eq u,Enum t,Bounded t,ElfEnum t u) => u -> Maybe t
toElfEnum_default u = case [x | x <- [minBound..maxBound], fromElfEnum x == u] of
  h:_ -> Just h
  [] -> Nothing
class (Eq u,Ord t,Enum t,Bounded t) => ElfEnum t u | t -> u where
  fromElfEnum :: t -> u
  toElfEnum :: u -> Maybe t
  toElfEnum = toElfEnum_default

data Elf sys = Elf {
  e_type :: ElfType sys,
  e_entry :: ElfN sys,
  e_sections :: [ElfSection sys]
  }

data ElfSection sys = ElfSection {
  es_name :: String,
  es_contents :: ElfSectionContents sys
  }
data ElfSectionContents sys = ElfSymTab {
  esc_symbols :: Map String (String,ElfN sys)
  }
                          | ElfProgbits {
  esc_vaddr :: ElfN sys,
  esc_flags :: (Bool,Bool,Bool),
  esc_data :: Bytes
  }

deriving instance ElfShow sys => Show (ElfSection sys)
deriving instance ElfShow sys => Show (ElfSectionContents sys)
deriving instance ElfShow sys => Show (Elf sys)

enumEncode :: (ElfEnum t u, Serializable Bytes u) => t -> Builder
enumEncode x = encode (Proxy :: Proxy Bytes) (fromElfEnum x)
enumDatum :: (ElfEnum t u, Format Bytes u) => Parser Bytes t
enumDatum = datum >>= \x -> case toElfEnum x of
  Just a -> return a
  _ -> noParse

data ElfABI = EABI_Linux
            | EABI_SysV
            deriving (Eq,Ord,Bounded,Enum,Show)
instance ElfEnum ElfABI Word8 where
  fromElfEnum EABI_SysV = 0
  fromElfEnum EABI_Linux = 3
instance Serializable Bytes ElfABI where encode _ = enumEncode
instance Format Bytes ElfABI       where datum = enumDatum

data ElfBits = EB_32 | EB_64
             deriving (Eq,Ord,Bounded,Enum,Show)
instance ElfEnum ElfBits Word8 where
  fromElfEnum EB_32 = 1
  fromElfEnum EB_64 = 2
instance Serializable Bytes ElfBits where encode _ = enumEncode
instance Format Bytes ElfBits       where datum = enumDatum

data ElfOrder = EO_LSB | EO_MSB
              deriving (Eq,Ord,Bounded,Enum,Show)
instance ElfEnum ElfOrder Word8 where
  fromElfEnum EO_LSB = 1
  fromElfEnum EO_MSB = 2
instance Serializable Bytes ElfOrder where encode _ = enumEncode
instance Format Bytes ElfOrder       where datum = enumDatum
 
data ElfType sys = ET_Exec
               | ET_Rel
               | ET_Dyn
               deriving (Eq,Ord,Bounded,Enum,Show)
instance ElfSystem sys => ElfEnum (ElfType sys) (Elf16 sys) where
  fromElfEnum ET_Rel  = e_16 1
  fromElfEnum ET_Exec = e_16 2
  fromElfEnum ET_Dyn  = e_16 3
instance ElfSystem sys => Serializable Bytes (ElfType sys) where encode _ = enumEncode
instance ElfSystem sys => Format Bytes (ElfType sys) where datum = enumDatum
 
data ElfMachine sys = EM_X86 | EM_X86_64 | EM_ARM
                  deriving (Eq,Ord,Bounded,Enum,Show)
instance ElfSystem sys => ElfEnum (ElfMachine sys) (Elf16 sys) where
  fromElfEnum EM_X86 = e_16 3
  fromElfEnum EM_X86_64 = e_16 62
  fromElfEnum EM_ARM = e_16 40
instance ElfSystem sys => Serializable Bytes (ElfMachine sys) where encode _ = enumEncode
instance ElfSystem sys => Format Bytes (ElfMachine sys) where datum = enumDatum
 
natSize :: (ElfSystem sys,Num n) => sys -> n
natSize sys = case b of EB_32 -> 4 ; EB_64 -> 8
  where (_,_,b,_) = e_abi sys
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
instance Serializable Bytes ElfIdent
instance Format Bytes ElfIdent

data ElfHeader sys = ElfHeader {
  -- ident
  eh_ident :: ElfIdent,
  eh_type :: ElfType sys,
  eh_machine :: ElfMachine sys,
  eh_fileVersion :: Elf32 sys, -- equals 1
  eh_entry, eh_phoff, eh_shoff :: ElfN sys,
  eh_flags :: Elf32 sys,
  eh_ehsize, eh_phentsize, eh_phnum, eh_shentsize, eh_shnum, eh_shstrndx :: Elf16 sys
  }
                 deriving Generic
deriving instance ElfShow sys => Show (ElfHeader sys)
instance ElfSystem sys => Serializable Bytes (ElfHeader sys)
instance ElfSystem sys => Format Bytes (ElfHeader sys)

data ElfPHType sys = PHT_Load
                 | PHT_Dynamic
                 | PHT_Interp
                 | PHT_Note
                 | PHT_GNU_Stack
                 | PHT_GNU_RelRO
                 | PHT_GNU_EH_Frame
                 deriving (Eq,Ord,Enum,Bounded,Show)
instance ElfSystem sys => ElfEnum (ElfPHType sys) (Elf32 sys) where
  fromElfEnum PHT_Load          = e_32 1
  fromElfEnum PHT_Dynamic       = e_32 2
  fromElfEnum PHT_Interp        = e_32 3
  fromElfEnum PHT_Note          = e_32 4
  fromElfEnum PHT_GNU_EH_Frame  = e_32 0x6474e550
  fromElfEnum PHT_GNU_Stack     = e_32 0x6474e551
  fromElfEnum PHT_GNU_RelRO     = e_32 0x6474e552
instance ElfSystem sys => Serializable Bytes (ElfPHType sys) where encode _ = enumEncode
instance ElfSystem sys => Format Bytes (ElfPHType sys) where datum = enumDatum

data ElfProgramHeaderFlag sys = EPH_Read | EPH_Write | EPH_Exec
                              deriving (Eq,Ord,Enum,Bounded,Show)
instance ElfSystem sys => ElfEnum (ElfProgramHeaderFlag sys) (Elf32 sys) where
  fromElfEnum EPH_Exec = e_32 1
  fromElfEnum EPH_Write = e_32 2
  fromElfEnum EPH_Read = e_32 4
data ElfProgramHeader sys = ElfProgramHeader {
  eph_type :: ElfPHType sys,
  eph_flags :: ElfFlags (ElfProgramHeaderFlag sys),
  eph_offset, eph_vaddr, eph_paddr,
  eph_filesz, eph_memsz, eph_align :: ElfN sys
  }
deriving instance ElfShow sys => Show (ElfProgramHeader sys)
instance ElfSystem sys => Serializable Bytes (ElfProgramHeader sys) where
  encode p (ElfProgramHeader t f o va pa fs ms al) = case bits of
    EB_32 -> encode p (t,o,va,pa,fs,ms,f,al)
    EB_64 -> encode p (t,f,o,va,pa,fs,ms,al)
    where (_,_,bits,_) = e_abi (undefined :: sys)
instance ElfSystem sys => Format Bytes (ElfProgramHeader sys) where
  datum = case bits of
    EB_32 -> datum <&> \(t,o,va,pa,fs,ms,f,al) -> ElfProgramHeader t f o va pa fs ms al
    EB_64 -> datum <&> \(t,f,o,va,pa,fs,ms,al) -> ElfProgramHeader t f o va pa fs ms al
    where (_,_,bits,_) = e_abi (undefined :: sys)
 
data ElfSHType sys = SHT_SymTab
                 | SHT_ProgBits
                 | SHT_NoBits
                 | SHT_StrTab
                 | SHT_Rel
                 | SHT_Rela
                 | SHT_Hash
                 | SHT_Note
                 | SHT_Fini_Array
                 | SHT_Init_Array
                 | SHT_Dynamic
                 | SHT_GNU_Hash
                 | SHT_GNU_VerSym
                 | SHT_GNU_VerNeeded
                 | SHT_Null
                 | SHT_DynSym
               deriving (Eq,Ord,Bounded,Enum,Show)
instance ElfSystem sys => ElfEnum (ElfSHType sys) (Elf32 sys) where
  fromElfEnum SHT_Null       = e_32 0
  fromElfEnum SHT_ProgBits   = e_32 1
  fromElfEnum SHT_SymTab     = e_32 2
  fromElfEnum SHT_StrTab     = e_32 3
  fromElfEnum SHT_Rela       = e_32 4
  fromElfEnum SHT_Hash       = e_32 5
  fromElfEnum SHT_Dynamic    = e_32 6
  fromElfEnum SHT_Rel        = e_32 9
  fromElfEnum SHT_NoBits     = e_32 8
  fromElfEnum SHT_Note       = e_32 7
  fromElfEnum SHT_Fini_Array = e_32 15
  fromElfEnum SHT_Init_Array = e_32 14
  fromElfEnum SHT_DynSym     = e_32 11
  fromElfEnum SHT_GNU_Hash       = e_32 0x6ffffff6
  fromElfEnum SHT_GNU_VerSym     = e_32 0x6fffffff
  fromElfEnum SHT_GNU_VerNeeded  = e_32 0x6ffffffe
  
instance ElfSystem sys => Serializable Bytes (ElfSHType sys) where encode p = enumEncode
instance ElfSystem sys => Format Bytes (ElfSHType sys) where datum = enumDatum

data ElfSectionHeaderFlag sys = SHF_Write | SHF_Alloc | SHF_ExecInstr | SHF_MaskProc
                            deriving (Eq,Ord,Enum,Bounded,Show)
instance ElfSystem sys => ElfEnum (ElfSectionHeaderFlag sys) (ElfN sys) where
  fromElfEnum SHF_Write      = e_n 1
  fromElfEnum SHF_Alloc      = e_n 2
  fromElfEnum SHF_ExecInstr  = e_n 4
  fromElfEnum SHF_MaskProc   = e_n 0xf0000000

data ElfSectionHeader sys = ElfSectionHeader {
  esh_name :: Elf32 sys,
  esh_type :: ElfSHType sys,
  esh_flags :: ElfFlags (ElfSectionHeaderFlag sys),
  esh_addr, esh_offset, esh_size :: ElfN sys,
  esh_link, esh_info :: Elf32 sys,
  esh_addralign, esh_entsize :: ElfN sys
  }
                        deriving Generic
deriving instance ElfShow sys => Show (ElfSectionHeader sys)
instance ElfSystem sys => Serializable Bytes (ElfSectionHeader sys)
instance ElfSystem sys => Format Bytes (ElfSectionHeader sys)
nullSectionHeader :: ElfSystem sys => ElfSectionHeader sys
nullSectionHeader = ElfSectionHeader (e_32 0) SHT_Null (ElfFlags zero) (e_n 0) (e_n 0) (e_n 0) (e_32 0) (e_32 0) (e_n 0) (e_n 0)

data ElfSymType = EST_NoType
                | EST_Object
                | EST_Func
                | EST_File
                | EST_Section
                deriving (Eq,Ord,Bounded,Enum,Show)
instance ElfEnum ElfSymType Word8 where
  fromElfEnum EST_NoType   = 0
  fromElfEnum EST_Object   = 1
  fromElfEnum EST_Func     = 2
  fromElfEnum EST_Section  = 3
  fromElfEnum EST_File     = 4
data ElfSymBind = ESB_Local
                | ESB_Global
                | ESB_Weak
                | ESB_LoProc | ESB_HiProc
                deriving (Eq,Ord,Bounded,Enum,Show)
instance ElfEnum ElfSymBind Word8 where
  fromElfEnum ESB_Local    = 0
  fromElfEnum ESB_Global   = 1
  fromElfEnum ESB_Weak     = 2
  fromElfEnum ESB_LoProc   = 13
  fromElfEnum ESB_HiProc   = 15

data ElfSymInfo = ElfSymInfo {
  esyi_type :: ElfSymType,
  esyi_bind :: ElfSymBind
  }
                   deriving Show
instance Serializable Bytes ElfSymInfo where
  encode p (ElfSymInfo t b) = encode p (fromElfEnum t .|. (fromElfEnum b `shiftL` 4))
instance Format Bytes ElfSymInfo where
  datum = datum >>= \x -> case (toElfEnum (x .&. 0xf), toElfEnum (x`shiftR`4)) of
    (Just t,Just b) -> return (ElfSymInfo t b)
    _ -> noParse

data ElfSymVisibility = ESV_Default | ESV_Internal | ESV_Hidden | ESV_Protected
                      deriving (Eq,Ord,Enum,Bounded,Show)
instance ElfEnum ElfSymVisibility Word8 where
  fromElfEnum ESV_Default    = 0
  fromElfEnum ESV_Internal   = 1
  fromElfEnum ESV_Hidden     = 2
  fromElfEnum ESV_Protected  = 3
instance Serializable Bytes ElfSymVisibility where encode p = enumEncode
instance Format Bytes ElfSymVisibility       where datum = enumDatum
data ElfSym sys = ElfSym {
  esy_name :: Elf32 sys,
  esy_info :: ElfSymInfo,
  esy_visibility :: ElfSymVisibility,
  esy_shndx :: Elf16 sys,
  esy_value, esy_size :: ElfN sys
  }
              deriving Generic
deriving instance ElfShow sys => Show (ElfSym sys)
instance ElfSystem sys => Serializable Bytes (ElfSym sys) where
  encode p (ElfSym n i o ndx v sz) = case bits of
    EB_32 -> encode p (n,v,sz,i,o,ndx)
    EB_64 -> encode p (n,i,o,ndx,v,sz)
    where (_,_,bits,_) = e_abi (undefined :: sys)
instance ElfSystem sys => Format Bytes (ElfSym sys) where
  datum = case bits of
    EB_32 -> datum <&> \(n,v,sz,i,o,ndx) -> ElfSym n i o ndx v sz
    EB_64 -> datum <&> \(n,i,o,ndx,v,sz) -> ElfSym n i o ndx v sz
    where (_,_,bits,_) = e_abi (undefined :: sys)
nullSym :: ElfSystem sys => ElfSym sys
nullSym = ElfSym (e_32 0) (ElfSymInfo EST_NoType ESB_Local) ESV_Default (e_16 0) (e_n 0) (e_n 0)

data ElfRelType sys = ERT_None
                  | ERT_Direct8 | ERT_Direct16 | ERT_Direct32
                  | ERT_PC8 | ERT_PC16 | ERT_PC32
                  | ERT_Other
                  deriving (Eq,Ord,Enum,Bounded,Show)
instance ElfSystem sys => ElfEnum (ElfRelType sys) Word8 where
  fromElfEnum = case mac of
    EM_X86_64 -> encode_x86_64
    EM_X86 -> encode_x86
    EM_ARM -> encode_arm
    where
      (mac,_,_,_) = e_abi (undefined :: sys)
      encode_x86_64 ERT_None      = 0
      encode_x86_64 ERT_Direct8   = 14
      encode_x86_64 ERT_Direct16  = 12
      encode_x86_64 ERT_Direct32  = 10
      encode_x86_64 ERT_PC8       = 15
      encode_x86_64 ERT_PC16      = 13
      encode_x86_64 ERT_PC32      = 2
      encode_x86_64 ERT_Other     = 255

      encode_x86 ERT_None     = 0
      encode_x86 ERT_Direct8  = 22
      encode_x86 ERT_Direct16 = 20
      encode_x86 ERT_Direct32 = 1
      encode_x86 ERT_PC8      = 23
      encode_x86 ERT_PC16     = 21
      encode_x86 ERT_PC32     = 2
      encode_x86 ERT_Other    = 255

      encode_arm ERT_None      = 0
      encode_arm ERT_Direct8   = 8
      encode_arm ERT_Direct16  = 5
      encode_arm ERT_Direct32  = 2
      encode_arm ERT_PC8       = 0
      encode_arm ERT_PC16      = 0
      encode_arm ERT_PC32      = 0
      encode_arm ERT_Other     = 255
  toElfEnum x = Just (fromMaybe ERT_Other (toElfEnum_default x))

data ElfRelInfo sys = ElfRelInfo {
  eri_sym :: Elf32 sys,
  eri_type :: ElfRelType sys
  }
                  deriving Show
instance ElfSystem sys => Serializable Bytes (ElfRelInfo sys) where
  encode p (ElfRelInfo s t) = case bits of
    EB_32 -> encode p (e_32 $ (e_from32 s`shiftL`8) .&. fromIntegral (fromElfEnum t) :: Elf32 sys)
    EB_64 -> encode p (e_64 $ (fromIntegral (e_from32 s)`shiftL`32) .&. fromIntegral (fromElfEnum t) :: Elf64 sys)
    where (_,_,bits,_) = e_abi (undefined :: sys)
instance ElfSystem sys => Format Bytes (ElfRelInfo sys) where
  datum = case bits of
    EB_32 -> datum >>= \x -> case toElfEnum (fromIntegral (e_from32 x)) of
      Just t -> return $ ElfRelInfo (e_32 (e_from32 (x :: Elf32 sys)`shiftR`8)) t
      Nothing -> noParse
    EB_64 -> datum >>= \x -> case toElfEnum (fromIntegral (e_from64 x)) of
      Just t -> return $ ElfRelInfo (e_32 (fromIntegral (e_from64 (x :: Elf64 sys)`shiftR`32))) t
      Nothing -> noParse
    where (_,_,bits,_) = e_abi (undefined :: sys)
data ElfRel sys = ElfRel {
  er_offset :: ElfN sys,
  er_info :: ElfRelInfo sys
  }
              deriving Generic
deriving instance (ElfShow sys, ElfSystem sys) => Show (ElfRel sys)
instance ElfSystem sys => Serializable Bytes (ElfRel sys)
instance ElfSystem sys => Format Bytes (ElfRel sys)
data ElfRela sys = ElfRela {
  era_offset :: ElfN sys,
  era_info :: ElfRelInfo sys,
  era_addend :: ElfN sys
  }
               deriving Generic
deriving instance (ElfShow sys,ElfSystem sys) => Show (ElfRela sys)
instance ElfSystem sys => Serializable Bytes (ElfRela sys)
instance ElfSystem sys => Format Bytes (ElfRela sys)

instance forall sys. ElfSystem sys => Serializable Bytes (Elf sys) where
  encode p (Elf t entry progs) = ehdr + fold [by bytesBuilder dat
                                           | ElfSection { es_contents = ElfProgbits { esc_data = dat } } <- progs]
                               + fold pHdrs + fold sHdrs + by bytesBuilder strTable
                               + fold symtabs
    where vers :: Num n => n ; vers = 1
          enc16 n = encode p (e_16 n :: Elf16 sys)
          enc32 n = encode p (e_32 n :: Elf32 sys)
          encN n  = encode p (e_n n :: ElfN sys)
          sys = sys :: sys
          (m,a,b,o) = e_abi sys
          
          phNum     = fromIntegral (length [() | ElfSection { es_contents = ElfProgbits { } } <- progs]) :: Word16
          shNum     = fromIntegral (length progs) + 2                          :: Word16

          progOff = fromIntegral (ehSize sys)
          phOff = progOff + fromIntegral (fold [bytesSize dat | ElfSection { es_contents = ElfProgbits { esc_data = dat } } <- progs])
          shOff = phOff + fromIntegral (phEntSize sys) * fromIntegral phNum
          strTOff = shOff + fromIntegral (shEntSize sys) * fromIntegral shNum
          symTOff = strTOff + fromIntegral (bytesSize strTable)

          ehdr = encode p (ElfHeader
                         (ElfIdent eH_MAGIC b o 1 a 0 (0,0,0))
                         t m (e_32 1)
                         entry (e_n phOff) (e_n shOff)
                         (e_32 0)
                         (e_16 $ ehSize sys)
                         (e_16 $ phEntSize sys) (e_16 phNum)
                         (e_16 $ shEntSize sys) (e_16 shNum)
                         (e_16 1))
                           
          hdrOffs = mapAccum_ withOff progs (progOff,symTOff)
            where withOff p@(ElfSection _ (ElfProgbits _ _ dat)) (offP,offS) = ((off',offS),(offP,p))
                    where off' = offP+fromIntegral (bytesSize dat)
                  withOff p@(ElfSection _ (ElfSymTab syms)) (offP,offS) = ((offP,offS'),(offS,p))
                    where offS' = offS+fromIntegral (stEntSize sys * size syms)
          pHdrs = map pHdr hdrOffs
            where pHdr (off,ElfSection { es_contents = ElfProgbits vaddr (r,w,x) dat }) = encode p ElfProgramHeader {
                    eph_type = PHT_Load,
                    eph_flags = fromFlagList [(r,EPH_Read),(w,EPH_Write),(x,EPH_Exec)],
                    eph_offset = e_n off, eph_vaddr = vaddr, eph_paddr = e_n 0,
                    eph_filesz = e_n sz, eph_memsz = e_n sz,
                    eph_align = e_n 4096
                    }
                    where sz = fromIntegral (bytesSize dat)
                  pHdr _ = zero

          sHdrs = map (encode p) (nullSectionHeader : strTHdr : map sHdr hdrOffs :: [ElfSectionHeader sys])
            where sHdr (off,ElfSection nm (ElfProgbits vaddr (r,w,x) dat)) = nullSectionHeader {
                    esh_name = e_32 (strIndex nm),
                    esh_type = SHT_ProgBits,
                    esh_flags = fromFlagList [(w,SHF_Write),(r,SHF_Alloc),(x,SHF_ExecInstr)],
                    esh_addr = vaddr, esh_offset = e_n off, esh_size = e_n sz
                    }
                    where sz = fromIntegral (bytesSize dat)
                  sHdr (off,ElfSection nm (ElfSymTab syms)) = nullSectionHeader {
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

          symbols = map es_name progs + [s | ElfSection _ (ElfSymTab syms) <- progs, s <- keys syms]
          strIndex nm = fromIntegral (fromMaybe 0 (lookup nm nmInds))
            where nmInds = c'map $ fromAList $ mapAccum_ (\nm acc -> (acc+length nm+1,(nm,acc))) symbols 1
          strTable = stringBytes (foldMap ('\0':) symbols + "\0")

          symtabs = map stHdr hdrOffs
            where stHdr (_,ElfSection _ (ElfSymTab syms)) = fold [sym n s v | (n,(s,v)) <- syms^.ascList]
                  stHdr _ = zero
                  sym n s v = let shndx = head [i | (s',i) <- zip (map es_name progs) [2..], s==s']
                              in encode p nullSym { esy_visibility = ESV_Protected,
                                                    esy_name = e_32 (strIndex n),
                                                    esy_value = v,
                                                    esy_shndx = e_16 shndx }

instance ElfSystem sys => Enum (Elf16 sys) where
  toEnum n = e_16 (toEnum n)
  fromEnum x = fromEnum (e_from16 x)
instance ElfSystem sys => Num (Elf16 sys) where
  x + y = e_16 (e_from16 x P.+ e_from16 y)
  x * y = e_16 (e_from16 x P.* e_from16 y)
  abs x = e_16 (abs (e_from16 x))
  signum x = e_16 (signum (e_from16 x))
  negate x = e_16 (negate (e_from16 x))
  fromInteger n = e_16 (fromInteger n)
instance ElfSystem sys => Real (Elf16 sys) where
  toRational x = toRational (e_from16 x)
instance ElfSystem sys => Integral (Elf16 sys) where
  quotRem x y = (e_16<#>e_16) (quotRem (e_from16 x) (e_from16 y))
  toInteger x = toInteger (e_from16 x)

instance ElfSystem sys => Enum (Elf32 sys) where
  toEnum n = e_32 (toEnum n)
  fromEnum x = fromEnum (e_from32 x)
instance ElfSystem sys => Num (Elf32 sys) where
  x + y = e_32 (e_from32 x P.+ e_from32 y)
  x * y = e_32 (e_from32 x P.* e_from32 y)
  abs x = e_32 (abs (e_from32 x))
  signum x = e_32 (signum (e_from32 x))
  negate x = e_32 (negate (e_from32 x))
  fromInteger n = e_32 (fromInteger n)
instance ElfSystem sys => Real (Elf32 sys) where
  toRational x = toRational (e_from32 x)
instance ElfSystem sys => Integral (Elf32 sys) where
  quotRem x y = (e_32<#>e_32) (quotRem (e_from32 x) (e_from32 y))
  toInteger x = toInteger (e_from32 x)

instance ElfSystem sys => Enum (Elf64 sys) where
  toEnum n = e_64 (toEnum n)
  fromEnum x = fromEnum (e_from64 x)
instance ElfSystem sys => Num (Elf64 sys) where
  x + y = e_64 (e_from64 x P.+ e_from64 y)
  x * y = e_64 (e_from64 x P.* e_from64 y)
  abs x = e_64 (abs (e_from64 x))
  signum x = e_64 (signum (e_from64 x))
  negate x = e_64 (negate (e_from64 x))
  fromInteger n = e_64 (fromInteger n)
instance ElfSystem sys => Real (Elf64 sys) where
  toRational x = toRational (e_from64 x)
instance ElfSystem sys => Integral (Elf64 sys) where
  quotRem x y = (e_64<#>e_64) (quotRem (e_from64 x) (e_from64 y))
  toInteger x = toInteger (e_from64 x)

instance ElfSystem sys => Enum (ElfN sys) where
  toEnum n = e_n (toEnum n)
  fromEnum x = fromEnum (e_fromN x)
instance ElfSystem sys => Num (ElfN sys) where
  x + y = e_n (e_fromN x P.+ e_fromN y)
  x * y = e_n (e_fromN x P.* e_fromN y)
  abs x = e_n (abs (e_fromN x))
  signum x = e_n (signum (e_fromN x))
  negate x = e_n (negate (e_fromN x))
  fromInteger n = e_n (fromInteger n)
instance ElfSystem sys => Real (ElfN sys) where
  toRational x = toRational (e_fromN x)
instance ElfSystem sys => Integral (ElfN sys) where
  quotRem x y = (e_n<#>e_n) (quotRem (e_fromN x) (e_fromN y))
  toInteger x = toInteger (e_fromN x)

sHN_ABS :: Num n => n
sHN_ABS = 65521
instance forall sys. (ElfShow sys, ElfSystem sys) => Format Bytes (Elf sys) where
  datum = do
    file <- remaining
    elfH <- datum
    let atOffset :: Integral n => n -> Parser Bytes a -> Parser Bytes a
        atOffset off m = do
          old <- runStreamState (id <~ \x -> (drop (fromIntegral off) file,x))
          m <* runStreamState (put old)
        readNRecords :: forall a n en. Integral n => n -> Parser Bytes a -> Parser Bytes [a]
        readNRecords n m = for [1..toInteger n] $ \_ -> m
    trace (show elfH) unit
    secHdrs <- atOffset (eh_shoff elfH) $ do
      readNRecords (eh_shnum elfH) datum
    progHdrs <- atOffset (eh_phoff elfH) $ do
      readNRecords (eh_phnum elfH) datum
    addSections <- for (zip [0..] secHdrs) $ \case
      (i,ElfSectionHeader { esh_type = SHT_Rela, esh_size, esh_entsize, esh_offset, esh_info, esh_link }) -> do
        x <- atOffset esh_offset $ do
          readNRecords (esh_size `div` esh_entsize) datum
        trace (intercalate "\n" $ map show (x :: [ElfRela sys])) unit
        return $ warp (l'2.mat (fromIntegral esh_info).l'2) (+map (esh_link,) x)
      (i,ElfSectionHeader { esh_type = SHT_SymTab, esh_size, esh_entsize, esh_offset }) -> do
        x <- atOffset esh_offset $ readNRecords (e_fromN esh_size`div`e_fromN esh_entsize) datum
        trace (intercalate "\n" $ map show (x :: [ElfSym sys])) unit
        return $ compose [warp l'1 (insert (i,j) (fromIntegral esy_shndx,esy_value))
                         | (j,ElfSym { esy_shndx, esy_value, esy_info = ElfSymInfo { esyi_type } }) <- zip [0..] x
                         , esy_shndx /= sHN_ABS && (esyi_type `elem` [EST_Func,EST_Object,EST_Section])]
      (i,ElfSectionHeader { esh_type = SHT_ProgBits, esh_flags, esh_offset, esh_size }) | isFlagSet SHF_Alloc esh_flags -> do
        x <- atOffset esh_offset $ runStreamState (id <~ swap . splitAt (fromIntegral esh_size))
        trace (show x) unit
        return $ set (l'2.mat (i :: Int).l'1) x
      _ -> return id
    let (syms,secs) = compose addSections (zero :: (Map (Int,Int) (Int,ElfN sys),Map Int (Bytes,[(Elf32 sys,ElfRela sys)])))
        secs' :: Map Int ([(Bytes,ElfRelType sys,Maybe (Int,ElfN sys))],Bytes)
        secs' = map (uncurry makeRelative >>> \(x,y,z) -> (reverse z,y)) secs
          where makeRelative bs rels = foldl' (\(cur,base,rest)
                                                (off,(typ,symt,sym))
                                               -> let (h,t) = splitAt (fromIntegral (off P.- cur)) base
                                                  in (off,dropRel typ t,(h,typ,lookup (fromIntegral symt,fromIntegral sym) syms):rest))
                                       (0,bs,[])
                                       $ ascList $^ c'map
                                       $ fromAList [(era_offset,(eri_type,symt,eri_sym))
                                                   | (symt,ElfRela { era_info = ElfRelInfo { eri_type, eri_sym }
                                                                   , era_offset }) <- rels
                                                   , eri_type /= ERT_Other]
                dropRel ERT_PC32 = drop 4
                dropRel ERT_PC16 = drop 2
                dropRel ERT_PC8  = drop 1
                dropRel ERT_Direct32 = drop 4
                dropRel ERT_Direct16 = drop 2
                dropRel ERT_Direct8 = drop 1
                dropRel _ = id
    trace (show syms +"\n"+ show secs + "\n" + show secs') unit  
    let x = secHdrs :: [ElfSectionHeader sys]; y = progHdrs :: [ElfProgramHeader sys]
    trace (intercalate "\n" $ map show secHdrs + map show progHdrs) $ return ()
    return (Elf (eh_type elfH) (eh_entry elfH) [])
