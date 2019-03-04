module Curly.Style(
  -- * Posix terminals for more entertaining documentation rendering
  ANSITerm,setupTerm,setupTermFromEnv) where
 
import Definitive
import Curly.Core.Documentation
import qualified Control.Monad as Mon
import qualified Prelude as P
import System.Environment (lookupEnv)

csi = "\x1b["

data ANSITerm = ColorTerm | BWTerm | DummyTerm
data Layer = Fg | Bg
tc2c :: Layer -> TermColor -> String
tc2c Fg Black = "30m"
tc2c Bg Black = "40m"
tc2c Fg Red = "31m"
tc2c Bg Red = "41m"
tc2c Fg Green = "32m"
tc2c Bg Green = "42m"
tc2c Fg Yellow = "33m"
tc2c Bg Yellow = "43m"
tc2c Fg Blue = "34m"
tc2c Bg Blue = "44m"
tc2c Fg Magenta = "35m"
tc2c Bg Magenta = "45m"
tc2c Fg Cyan = "36m"
tc2c Bg Cyan = "46m"
tc2c Fg White = "37m"
tc2c Bg White = "47m"
tc2c Fg (ColorNumber n) = "38;5;"+show n+"m"
instance Terminal ANSITerm where
  setBold DummyTerm _                   = ""
  setBold _ b                           = csi + if b then "1m" else "m"
  setUnderlined DummyTerm _             = ""
  setUnderlined _ b                     = csi + if b then "4m" else "24m"
  setItalic DummyTerm _                 = ""
  setItalic _ b                         = csi + if b then "3m" else "23m"
  setForegroundColor ColorTerm c        = csi + tc2c Fg c
  setForegroundColor _ _                = ""
  setBackgroundColor ColorTerm c        = csi + tc2c Bg c
  setBackgroundColor _ _                = ""
  restoreDefaultColors ColorTerm        = csi + "39m"
  restoreDefaultColors _                = ""

setupTerm :: String -> IO ANSITerm
setupTerm "vt100" = return BWTerm
setupTerm ('e':'t':'e':'r':'m':'-':_) = return BWTerm
setupTerm "" =  return DummyTerm
setupTerm _ = return ColorTerm
setupTermFromEnv :: IO ANSITerm
setupTermFromEnv = setupTerm . fromMaybe "" =<< lookupEnv "TERM" 
