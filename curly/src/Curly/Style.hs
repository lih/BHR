module Curly.Style(
  -- * Posix terminals for more entertaining documentation rendering
  ANSITerm,setupTerm,setupTermFromEnv) where
 
import Definitive
import Curly.Core.Documentation
import qualified Control.Monad as Mon
import qualified Prelude as P
import System.Environment (lookupEnv)

csi = "\x1b["

data ANSITerm = ANSITerm Bool
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
  setBold _ b             = csi + if b then "1m" else "m"
  setUnderlined _ b       = csi + if b then "4m" else "24m"
  setItalic _ b           = csi + if b then "3m" else "23m"
  setForegroundColor (ANSITerm True) c = csi + tc2c True c
  setForegroundColor _ _ = ""
  setBackgroundColor (ANSITerm True) c = csi + tc2c False c
  setBackgroundColor _ _ = ""
  restoreDefaultColors (ANSITerm True) = csi + "39m"
  restoreDefaultColors _ = ""

setupTerm :: String -> IO ANSITerm
setupTerm "vt100" = return (ANSITerm False)
setupTerm ('e':'t':'e':'r':'m':'-':_) = return (ANSITerm False)
setupTerm _ =  return (ANSITerm True)
setupTermFromEnv :: IO ANSITerm
setupTermFromEnv = setupTerm . fromMaybe "vt100" =<< lookupEnv "TERM" 
