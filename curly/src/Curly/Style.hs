module Curly.Style(
  -- * Writing documentation out
  setupTerm,setupTermFromEnv) where
 
import Definitive
import Curly.Core.Documentation
import qualified Control.Monad as Mon
import qualified Prelude as P
import qualified System.Console.Terminfo as TI
 
instance Semigroup (TI.Capability a) where (+) = Mon.mplus
instance Monoid (TI.Capability a) where zero = Mon.mzero
instance Semigroup TI.TermOutput where (+) = TI.mappend
instance Monoid TI.TermOutput where zero = TI.mempty

instance Functor TI.Capability where map = P.fmap
instance Unit TI.Capability where pure = P.return
instance SemiApplicative TI.Capability
instance Applicative TI.Capability
instance Monad TI.Capability where join x = x P.>>= id

newtype POSIXTerm = POSIXTerm TI.Terminal
runCap :: TI.Terminal -> TI.Capability TI.TermOutput -> String
runCap t c = maybe "" TI.termOutputString $ TI.getCapability t c
tc2c :: TermColor -> TI.Color
tc2c Black = TI.Black
tc2c Red = TI.Red
tc2c Yellow = TI.Yellow
tc2c Blue = TI.Blue
tc2c Magenta = TI.Magenta
tc2c Cyan = TI.Cyan
tc2c White = TI.White
tc2c (ColorNumber n) = TI.ColorNumber n
instance Terminal POSIXTerm where
  setBold (POSIXTerm t) b             = runCap t $ if b then TI.boldOn else TI.allAttributesOff
  setUnderlined (POSIXTerm t) b       = runCap t $ if b then TI.enterUnderlineMode else TI.exitUnderlineMode
  setItalic (POSIXTerm t) b           = runCap t $ if b then TI.tiGetOutput1 "sitm" else TI.tiGetOutput1 "ritm"
  setForegroundColor (POSIXTerm t) c  = runCap t $ TI.setForegroundColor <&> ($tc2c c)
  setBackgroundColor (POSIXTerm t) c  = runCap t $ TI.setBackgroundColor <&> ($tc2c c)
  restoreDefaultColors (POSIXTerm t)  = runCap t TI.restoreDefaultColors

setupTerm t = POSIXTerm <$> TI.setupTerm t
setupTermFromEnv = POSIXTerm <$> TI.setupTermFromEnv
