module Curly.Style(
  -- * Posix terminals for more entertaining documentation rendering
  ANSITerm,setupTerm,setupTermFromEnv) where
 
import Definitive
import Curly.Core.Documentation
import qualified Control.Monad as Mon
import qualified Prelude as P
 
csi = "\x1b["

data ANSITerm = ANSITerm
tc2c :: Bool -> TermColor -> String
tc2c True Black = "30m"
tc2c False Black = "40m"
tc2c True Red = "31m"
tc2c False Red = "41m"
tc2c True Green = "32m"
tc2c False Green = "42m"
tc2c True Yellow = "33m"
tc2c False Yellow = "43m"
tc2c True Blue = "34m"
tc2c False Blue = "44m"
tc2c True Magenta = "35m"
tc2c False Magenta = "45m"
tc2c True Cyan = "36m"
tc2c False Cyan = "46m"
tc2c True White = "37m"
tc2c False White = "47m"
tc2c True (ColorNumber n) = "38;5;"+show n+"m"
instance Terminal ANSITerm where
  setBold ANSITerm b             = csi + if b then "1m" else "m"
  setUnderlined ANSITerm b       = csi + if b then "4m" else "24m"
  setItalic ANSITerm b           = csi + if b then "3m" else "23m"
  setForegroundColor ANSITerm c  = csi + tc2c True c
  setBackgroundColor ANSITerm c  = csi + tc2c False c
  restoreDefaultColors ANSITerm  = csi + "39m"

setupTerm :: String -> IO ANSITerm
setupTerm _ = return ANSITerm
setupTermFromEnv :: IO ANSITerm
setupTermFromEnv = return ANSITerm
