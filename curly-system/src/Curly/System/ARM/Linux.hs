module Curly.System.ARM.Linux where

import Definitive
import Curly.System.Base
import Curly.System.ARM.Common
import Curly.System.Format.ELF
import Language.Format

arm_memextend n = let ?sys = arm_machine in pushing [R_r7,R_r0] $ do
  R_r7 <-- (0x2d :: Int)
  R_r11 <-- (undefined :: Int)
  R_r0 <-- reg R_r11 ! Offset 0
  add R_r0 (n :: Int)
  reg R_r11 ! Offset 0 <-- R_r0
  arm_instr [Conditional C_AL (SVC 0)]

system = arm_sys "linux-arm" prog
  where prog = Standalone $ \mtext -> do
          BA start <- mute $ rawProgram [TextSection,DataSection] $ do
            rtSection TextSection .l'2 =- BA pstart
            mtext
          rt <- get
          let BA dstart = rt^.rtSection TextSection .l'2
          tell $ bytesCode' $ serialize $ Elf ET_Exec (Linux_ARM_ElfN (fromIntegral start)) [
            ElfProgram "text" (Linux_ARM_ElfN (fromIntegral pstart)) (True,True,True) (rt^.rtSection TextSection .l'1.bData),
            ElfProgram "data" (Linux_ARM_ElfN (fromIntegral dstart)) (True,True,False) (rt^.rtSection DataSection .l'1.bData)]
            
        pstart = 0x400000 + fromIntegral (ehSize linux_arm + phEntSize linux_arm)
        
