
module Curly.Style(
  -- * Styles
  ClassStyle(..),Color(..),Display(..),Style,defaultStyle,
  classColor,classDisplay,classIsBold,classIsUnderlined,classIndent,classPrefix,classIsItalic,
  
  -- * Writing documentation out
  docString,setupTerm,setupTermFromEnv) where

import Curly.Core.Library(DocNode(..),Documentation)
import Definitive
import qualified Control.Monad as Mon
import qualified Prelude as P
import qualified System.Console.Terminfo as Term
import System.Console.Terminfo hiding (Monoid(..))

instance Semigroup (Capability a) where (+) = Mon.mplus
instance Monoid (Capability a) where zero = Mon.mzero
instance Semigroup TermOutput where (+) = Term.mappend
instance Monoid TermOutput where zero = Term.mempty

instance Functor Capability where map = P.fmap
instance Unit Capability where pure = P.return
instance SemiApplicative Capability
instance Applicative Capability
instance Monad Capability where join x = x P.>>= id

data ShowState = BeginP | InP | EndP Bool
data Display = Inline | Block Bool

data ClassStyle = ClassStyle {
  _classColor :: (Maybe Color,Maybe Color),
  _classDisplay :: Maybe Display,
  _classIsBold :: Maybe Bool,
  _classIsUnderlined :: Maybe Bool,
  _classIsItalic :: Maybe Bool,
  _classPrefix :: Maybe String,
  _classIndent :: Maybe Int
  }
classColor :: Lens' ClassStyle (Maybe Color,Maybe Color)
classColor = lens _classColor (\x y -> x { _classColor = y })
classIndent :: Lens' ClassStyle (Maybe Int)
classIndent = lens _classIndent (\x y -> x { _classIndent = y })
classPrefix :: Lens' ClassStyle (Maybe String)
classPrefix = lens _classPrefix (\x y -> x { _classPrefix = y })
classIsBold :: Lens' ClassStyle (Maybe Bool)
classIsBold = lens _classIsBold (\x y -> x { _classIsBold = y })
classIsItalic :: Lens' ClassStyle (Maybe Bool)
classIsItalic = lens _classIsItalic (\x y -> x { _classIsItalic = y })
classDisplay :: Lens' ClassStyle (Maybe Display)
classDisplay = lens _classDisplay (\x y -> x { _classDisplay = y })
classIsUnderlined :: Lens' ClassStyle (Maybe Bool)
classIsUnderlined = lens _classIsUnderlined (\x y -> x { _classIsUnderlined = y })

instance Semigroup ClassStyle where
  ClassStyle c bl bo u it p i + ClassStyle c' bl' bo' u' it' p' i' = ClassStyle (c'+c) (bl'+bl) (bo'+bo) (u'+u) (it'+it) (p'*p+p+p') (i'+i)
instance Monoid ClassStyle where
  zero = ClassStyle zero zero zero zero zero zero zero

type Style = Map String ClassStyle

defaultStyle :: Style
defaultStyle = fromAList [
  ("p",isBl zero),
  ("title",(isB . isBl) zero),
  ("nodoc",zero & classColor.l'1 %- Just (ColorNumber 67)),
  ("section",isBl zero),
  ("em",isB zero),
  ("ul",((classDisplay %- Just (Block True)) . (classIndent %- Just 2)) zero),
  ("li",((classDisplay %- Just (Block False)) . (classPrefix %- Just "* ")) zero),
  ("ln",set classDisplay (Just (Block False)) zero),
  ("sub",set classIndent (Just 2) zero)
  ]
  where isB = classIsBold %- Just True
        isBl = classDisplay %- Just (Block True)

docStr :: Style -> Documentation -> Capability TermOutput
docStr stl d = (doc' d^..i'RWST) ((),(BeginP,zero,0)) <&> \(_,_,t) -> t
  where doc' (Join (DocTag t as subs)) = do
          l'2.l'2 =~ compose [classDisplay %- Nothing,classIndent %- Nothing]
          pref <- saving l'2 $ saving l'3 $ do
            l'2 =~ \(_,s) -> (False,(s + fold [stl^.at c.folded | ("class",c) <- (("class",t):as)]))
            s <- getl (l'2.l'2)
            maybe unit (\i -> l'3 =~ (+i)) (s^.classIndent)
            maybe unit setDisplay (s^.classDisplay)
            case t of
              "nodoc" -> styleStart >> tell (termText "Not documented.")
              _ -> subDoc subs
            styleEnd
            getl (l'2.l'2.classPrefix)
          l'2 =~ (l'1 %- False) . (l'2.classPrefix %- pref)
          styleStart
        doc' (Pure t) = do
          st <- getl l'1
          case st of
            EndP b -> do
              tell (termText (if b then "\n\n" else "\n"))
              l'1 =- BeginP
            InP -> tell (termText " ")
            _ -> unit
          styleStart
          tell (termText t)
          l'1 =- InP
        subDoc docs = traverse_ doc' docs

        boolSt b k = maybe unit (\x -> if x then k else unit) b
        styleStart = do
          (isSet,ClassStyle (cf,cb) bl bo u it p _) <- getl l'2
          unless isSet $ do
            l'2.l'1 =- True
            maybe unit setDisplay bl
            tellC restoreDefaultColors
            maybe unit (setColor setForegroundColor) cf
            maybe unit (setColor setBackgroundColor) cb
            boolSt bo (setBold True) ; boolSt u (setUnderlined True)
            boolSt it (setItalic True)
            indent
            maybe unit (\pre -> addPrefix pre >> (l'2.l'2.classPrefix =- Nothing)) p
        styleEnd = do
          (isSet,ClassStyle (fg,bg) bl bo u it _ _) <- getl l'2
          when isSet $ do
            maybe unit endDisplay bl
            boolSt bo (setBold False) ; boolSt u (setUnderlined False)
            boolSt it (setItalic False)
            maybe unit (const (tellC restoreDefaultColors)) (fg+bg)
        tellC c = lift c >>= tell

        setColor gnd c = tellC (gnd <&> ($c))
        addPrefix p = tell (termText p) >> (l'3 =~ (+ length p))
        indent = getl l'1 >>= \st -> case st of
          BeginP -> getl l'3 >>= \n -> tell (termText (take n (repeat ' ')))
          _ -> unit

        bType b st = b || case st of EndP x -> x ; _ -> False
        setDisplay (Block b) = getl l'1 >>= \st -> do
          case st of
            BeginP -> unit
            _ -> l'1 =- EndP (bType b st)
        setDisplay _ = unit
        endDisplay (Block b) = l'1 =~ \st' -> EndP (bType b st')
        endDisplay _ = unit

        setBold b = tellC (if b then boldOn else allAttributesOff)
        setUnderlined b = tellC (if b then enterUnderlineMode else exitUnderlineMode)
        setItalic b = tellC $ (if b then tiGetOutput1 "sitm" else tiGetOutput1 "ritm") + pure zero
        
docString :: (?style :: Style) => Terminal -> Documentation -> String
docString t d = case getCapability t (docStr ?style d) of
  Just out -> termOutputString out
  _ -> error $ "Your terminal doesn't have the capabilities to show this documentation."
