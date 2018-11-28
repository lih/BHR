{-# LANGUAGE TypeFamilies #-}
module Curly.Core.Documentation(
  -- * The Documentation format
  DocNode(..),Documentation,Documented(..),
  docNodeAttrs,docNodeSubs,
  docTag,docTag',nodoc,mkDoc,showRawDoc,docAtom,docLine,docFormat,
  DocParams,DocPatterns,
  evalDoc,evalDocWithPatterns,
  -- * Rendering documentation
  -- ** Styles
  TagStyle(..),TermColor(..),TagDisplay(..),Style,defaultStyle,
  tagColor,tagDisplay,tagIsBold,tagIsUnderlined,tagIndent,tagPrefix,tagIsItalic,tagWordWrap,
  -- ** Rendering
  Terminal(..),DummyTerminal(..),docString,pretty,
  -- * Metadata
  Metadata(..),i'Metadata,
  -- * Formatted Strings
  FormatArg(..),FormatType(..),format,
  ) where

import Definitive
import Language.Format hiding (letter)
import IO.Network.Socket (PortNumber)
import System.Environment (lookupEnv)

-- | A documentation node (similar to a HTML node, but simpler)
data DocNode a = DocTag String [(String,String)] [a]
               deriving (Eq,Ord,Show,Generic)
instance Serializable Bytes a => Serializable Bytes (DocNode a)
instance Serializable Bytes a => Serializable Bytes (Free DocNode a) where encode = encodeFree
instance Format Bytes a => Format Bytes (DocNode a)
instance Format Bytes a => Format Bytes (Free DocNode a) where datum = datumFree
instance Functor DocNode where map f (DocTag t a xs) = DocTag t a (map f xs)
instance Foldable DocNode where fold (DocTag _ _ l) = fold l
instance Traversable DocNode where sequence (DocTag t as l) = DocTag t as<$>sequence l
docNodeAttrs :: Lens' (DocNode a) [(String,String)]
docNodeAttrs = lens (\(DocTag _ as _) -> as) (\(DocTag t _ s) as -> DocTag t as s)
docNodeSubs :: Lens [a] [b] (DocNode a) (DocNode b)
docNodeSubs = lens (\(DocTag _ _ x) -> x) (\(DocTag t as _) x -> DocTag t as x)
docTag :: String -> [(String,String)] -> [Free DocNode a] -> Free DocNode a
docTag t as subs = Join (DocTag t as subs)
docTag' :: String -> [Free DocNode a] -> Free DocNode a
docTag' t = docTag t []

type Documentation = Free DocNode String
class Documented t where
  document :: t -> Documentation
instance Documented a => Documented (Free DocNode a) where
  document = join . map document
instance Documented String where
  document = Pure
instance Documented Int where
  document n = docTag' "int" [Pure (show n)]

newtype Metadata = Metadata (Forest (Map String) String)
                 deriving (Semigroup,Monoid,Serializable Bytes)
i'Metadata :: Iso' (Forest (Map String) String) Metadata
i'Metadata = iso Metadata (\(Metadata m) -> m)
instance Format Bytes Metadata where datum = coerceDatum Metadata
instance DataMap Metadata String (Free (Map String) String) where 
  at i = from i'Metadata.at i
instance Show Metadata where
  show = \(Metadata m) -> showM m
    where showM m = format "{%s}" (intercalate " " [format "%s:%s" (show a) (showV v)
                                                   | (a,v) <- m^.ascList])
          showV (Pure s) = show s
          showV (Join m) = showM m
instance Read Metadata where
  readsPrec _ = readsParser (map Metadata brack)
    where val = map Pure readable <+? map Join brack
          brack = fromAList <$> between (single '{') (single '}') (sepBy' field (single ' '))
            where field = liftA2 (,) readable (single ':' >> val)
instance Documented Metadata where
  document m = Pure (show m)

type DocParams = Forest (Map String) Documentation
type DocPatterns = Map String ([String],Documentation)
evalDocWithPatterns :: DocPatterns -> DocParams -> Documentation -> Maybe Documentation
evalDocWithPatterns pats = eval
  where eval vars = eval'
          where 
            eval' (Pure x) = return (Pure x)
            eval' (Join (DocTag "$" [] xs)) = do
              xs' <- traverse eval' xs
              path <- for xs' $ \x -> x^?t'Pure
              Join vars^?at path.t'Just.t'Pure
            eval' (Join (DocTag "$*" [] xs)) = do
              xs' <- traverse eval' xs
              path <- for xs' $ \x -> x^?t'Pure
              v <- Join vars^?at path.t'Just.t'Join
              return (Pure $ show (Metadata $ map2 pretty v))
            eval' (Join (DocTag "env" [] [x])) = do
              x' <- eval' x
              Pure <$> lookupEnv (pretty x')^.thunk
            eval' (Join (DocTag "or" [] xs)) = foldMap eval' xs
            eval' (Join (DocTag "when" [] [x,y])) = eval' x >> eval' y
            eval' (Join (DocTag "unless" [] [x,y])) = maybe (Just ()) (const Nothing) (eval' x) >> eval' y
            eval' (Join (DocTag "splice" as xs)) = Join . DocTag "splice" as . foldr merge [] <$> traverse eval' xs
              where merge x [] = [x]
                    merge (Pure x) (Pure y:t) = Pure (x+y):t
                    merge x t = x:t
            eval' (Join (DocTag op [] [ea,eb]))
              | op`elem`["=","<",">","<=",">="] = do
                let valList = many' (map Left number <+? map Right (many1' (satisfy (not . inRange '0' '9'))))
                    liftOp cmp x@(Pure a) (Pure b) = x <$ do
                      [a',b'] <- traverse (matches Just valList) [a,b]
                      guard (cmp a' b')
                    liftOp cmp x@(Join (DocTag a _ xs)) (Join (DocTag b _ ys)) = x <$ do
                      guard (a==b)
                      sequence_ (zipWith (liftOp cmp) xs ys)
                    liftOp _ _ _ = Nothing
                    toCmp :: String -> [Integer :+: String] -> [Integer :+: String] -> Bool
                    toCmp "=" = (==)
                    toCmp "<" = (<)
                    toCmp ">" = (>)
                    toCmp "<=" = (<=)
                    toCmp ">=" = (>=)
                    toCmp _ = undefined
                join $ liftA2 (liftOp (toCmp op)) (eval' ea) (eval' eb)
              | op=="matches" = do
                let cmp (Pure a) (Pure b) = Pure a <$ matches Just (wildcards b) a
                    cmp (Join (DocTag a _ xs)) (Join (DocTag b _ ys)) = do
                      guard (a==b)
                      zs <- sequence (zipWith cmp xs ys)
                      return (Join $ DocTag a [] zs)
                    cmp _ _ = Nothing
                join $ liftA2 cmp (eval' ea) (eval' eb)
            eval' (Join (DocTag "call" _ xs@(_:_))) = do
              p:args <- traverse eval' xs
              pname <- p^?t'Pure
              (pargs,pat) <- pats^.at pname
              callTag args pargs pat
            eval' (Join (DocTag t as xs)) = do
              xs' <- traverse eval' xs
              case pats^.at t of
                Just (pargs,pat) -> callTag xs' pargs pat
                Nothing -> return (Join $ DocTag t as xs')
            callTag args pargs pat = do
              let vars' = compose (zipWith (\n v -> insert n (Pure v)) pargs args) vars
              eval vars' pat

        wildcards "*" = unit
        wildcards ('*':'*':t) = wildcards ('*':t)
        wildcards ('*':t@(c:_)) = do
          _ <- skipMany' (satisfy (/=c))`sepBy`single c
          wildcards t
        wildcards ('?':t) = token >> wildcards t
        wildcards (c:t) = single c >> wildcards t
        wildcards [] = eoi
        
evalDoc :: DocParams -> Documentation -> Maybe Documentation
evalDoc = evalDocWithPatterns zero

nodoc :: String -> Documentation
nodoc msg = Join (DocTag "nodoc" [] [Pure msg])
mkDoc :: String    -- ^ The root tag name
         -> String -- ^ Documentation in textual format
         -> Documentation
mkDoc t d = Join . DocTag t [] $ fromMaybe [] $ matches Just (between spc spc (sepBy' docAtom spc)) d
spc :: (ParseStream s, StreamChar s ~ Char,Monad m) => ParserT s m ()
spc = skipMany' (oneOf " \t\n")

docAtom :: (ParseStream s, StreamChar s ~ Char,Monad m) => ParserT s m Documentation
docFormat :: (ParseStream s, StreamChar s ~ Char,Monad m) => String -> [Char] -> ParserT s m Documentation
docAtom = fst docAtom'
docFormat = snd docAtom'

docAtom' :: (ParseStream s, StreamChar s ~ Char,Monad m) =>
           (ParserT s m Documentation,String -> [Char] -> ParserT s m Documentation)
docAtom' = (tag <+? txt,strSplice)
  where letter p = token >>= \case
          '\\' -> token
          c | (c`isKeyIn`reserved) || not (p c) -> zero
            | otherwise -> return c
        reserved = c'set (fromKList " \t\n{}\\")
        nameTo cs = many1' (letter (\c -> not (c`isKeyIn`res)))
          where res = c'set (fromKList (cs+"+.:#\""))
        attrName = nameTo "="
        tagName = nameTo ""
        attribute = (single ':' >> liftA2 (,) attrName (single '=' >> quotedString '"'))
                    <+? (single '.' >> tagName <&> ("class",))
                    <+? (single '#' >> tagName <&> ("id",))
        tag = between (single '{') (single '}') $ between spc spc $ do
          (tn,an):ns <- liftA2 (,) tagName (many' attribute) `sepBy1'` single '+'
          subs <- spc >> sepBy' docAtom spc
          return (Join $ DocTag tn an $ foldr (\(t,attrs) sub -> [Join $ DocTag t attrs sub]) subs ns)
        txt = (Pure <$> many1' (letter (/='"'))) <+? between (single '"') (single '"') (strSplice "splice" "\"")
        strSplice tname delim = do
          h <- option' id ((:) . Left <$> many1' strChar)
          t <- many' (map Left (many1' strChar) <+? map Right tag)
          return $ case h t of
            [Left s] -> Pure s
            l -> Join $ DocTag tname [] (map (Pure <|> id) l)
          where strChar = satisfy (\x -> not (x`elem`(delim+"{\\")))
                          <+? single '\\' >> token


docLine :: (ParseStream s, StreamChar s ~ Char, Monad m)
           => String -> [(String,String)] -> ParserT s m Documentation
docLine n as = Join . DocTag n as <$> many1' (skipMany' (oneOf " \t") >> docAtom)
showRawDoc :: Documentation -> String
showRawDoc = \case
  Join (DocTag t as xs) -> "{" + foldMap quoteChar t + foldMap showAttr as + foldMap showSub xs + "}"
  Pure s -> foldMap quoteChar s
  where quoteChar ' ' = "\\ "
        quoteChar c = [c]
        showAttr (x,v) = ":" + foldMap quoteChar x + "=" + foldMap quoteChar v
        showSub x = " "+showRawDoc x

data ShowState = BeginP | InP Bool | EndP Bool
data TagDisplay = Inline | Block Bool
data TermColor = Black     
               | Red     
               | Green     
               | Yellow     
               | Blue     
               | Magenta     
               | Cyan     
               | White     
               | ColorNumber Int     
 
data TagStyle = TagStyle {
  _tagColor :: (Maybe TermColor,Maybe TermColor),
  _tagDisplay :: Maybe TagDisplay,
  _tagIsBold :: Maybe Bool,
  _tagIsUnderlined :: Maybe Bool,
  _tagIsItalic :: Maybe Bool,
  _tagPrefix :: Maybe String,
  _tagIndent :: Maybe Int,
  _tagIsRawText :: Maybe Bool,
  _tagWordWrap :: Maybe Int
  }
instance Semigroup TagStyle where
  TagStyle c bl bo u it p i r w + TagStyle c' bl' bo' u' it' p' i' r' w' = TagStyle (c'+c) (bl'+bl) (bo'+bo) (u'+u) (it'+it) (p'*p+p+p') (i'+i) (r'+r) (w'+w)
instance Monoid TagStyle where
  zero = TagStyle zero zero zero zero zero zero zero zero zero

type Style = Map String TagStyle
 
tagColor :: Lens' TagStyle (Maybe TermColor,Maybe TermColor)
tagColor = lens _tagColor (\x y -> x { _tagColor = y })
tagIndent :: Lens' TagStyle (Maybe Int)
tagIndent = lens _tagIndent (\x y -> x { _tagIndent = y })
tagPrefix :: Lens' TagStyle (Maybe String)
tagPrefix = lens _tagPrefix (\x y -> x { _tagPrefix = y })
tagIsBold :: Lens' TagStyle (Maybe Bool)
tagIsBold = lens _tagIsBold (\x y -> x { _tagIsBold = y })
tagIsItalic :: Lens' TagStyle (Maybe Bool)
tagIsItalic = lens _tagIsItalic (\x y -> x { _tagIsItalic = y })
tagDisplay :: Lens' TagStyle (Maybe TagDisplay)
tagDisplay = lens _tagDisplay (\x y -> x { _tagDisplay = y })
tagIsUnderlined :: Lens' TagStyle (Maybe Bool)
tagIsUnderlined = lens _tagIsUnderlined (\x y -> x { _tagIsUnderlined = y })
tagIsRawText :: Lens' TagStyle (Maybe Bool)
tagIsRawText = lens _tagIsRawText (\x y -> x { _tagIsRawText = y })
tagWordWrap :: Lens' TagStyle (Maybe Int)
tagWordWrap = lens _tagWordWrap (\x y -> x { _tagWordWrap = y })

defaultStyle :: Style
defaultStyle = fromAList $ map (second ($zero)) $ [
  ("p",isBl),
  ("title",isB . isBl),
  ("nodoc",set (tagColor.l'1) (Just (ColorNumber 67))),
  ("section",isBl),
  ("em",isB),
  ("ul",set tagDisplay (Just (Block True)) . set tagIndent (Just 2)),
  ("li",set tagDisplay (Just (Block False)) . set tagPrefix (Just "- ")),
  ("modDir",set tagPrefix (Just "* ")),
  ("ln",set tagDisplay (Just (Block False))),
  ("sub",set tagIndent (Just 2)),
  ("splice",set tagIsRawText (Just True))
  ]
  where isB = tagIsBold %- Just True
        isBl = tagDisplay %- Just (Block True)
 
class Terminal trm where
  setBold              :: trm -> Bool -> String
  setUnderlined        :: trm -> Bool -> String
  setItalic            :: trm -> Bool -> String
  setForegroundColor   :: trm -> TermColor -> String
  setBackgroundColor   :: trm -> TermColor -> String
  restoreDefaultColors :: trm -> String
data DummyTerminal = DummyTerminal
instance Terminal DummyTerminal where
  setBold _ _             = ""
  setUnderlined _ _       = ""
  setItalic _ _           = ""
  setForegroundColor _ _  = ""
  setBackgroundColor _ _  = ""
  restoreDefaultColors _  = ""

data StyleState = StyleState {
  _showState :: ShowState,
  _activeStyle :: (Bool,TagStyle),
  _indentDepth :: Int,
  _column :: Int
  }
showState :: Lens' StyleState ShowState
showState = lens _showState (\x y -> x { _showState = y })
activeStyle :: Lens' StyleState (Bool,TagStyle)
activeStyle = lens _activeStyle (\x y -> x { _activeStyle = y })
indentDepth :: Lens' StyleState Int
indentDepth = lens _indentDepth (\x y -> x { _indentDepth = y })
column :: Lens' StyleState Int
column = lens _column (\x y -> x { _column = y })

docString :: Terminal trm => trm -> Style -> Documentation -> String
docString trm stl d = getId ((doc' d^..i'RWST) ((),StyleState BeginP zero 0 0)) & \(_,_,t) -> t
  where addStyles s s' = (s+s') & set tagPrefix (s'^.tagPrefix + s^.tagPrefix) 
        tagStl t as = foldl' addStyles zero [stl^.at c.folded | ("class",c) <- (("class",t):as)]
        tellText txt = do
          ind <- getl indentDepth
          wrp <- getl (activeStyle.l'2.tagWordWrap)
          col <- getl column
          let indtxt = withIndent ind txt
              newtxt = if maybe False (\w -> adjustCol indtxt col > w) wrp
                       then '\n':take ind (repeat ' ')+dropWhile (`elem`" \t") indtxt
                       else indtxt
          tell newtxt
          column =~ adjustCol newtxt
        adjustCol ('\n':t) _ = adjustCol t 0
        adjustCol (_:t) col = adjustCol t (col+1)
        adjustCol [] col = col
        doc' (Join (DocTag t as subs)) = do
          activeStyle.l'2 =~ compose [tagDisplay %- Nothing,tagIndent %- Nothing]
          pref <- saving activeStyle $ saving indentDepth $ do
            let tstl = tagStl t as
            activeStyle =~ \(_,s) -> (False,(s + tstl))
            s <- getl (activeStyle.l'2)
            indentDepth =~
              maybe id ((+) . length) (tstl^.tagPrefix)
              . maybe id (+) (s^.tagIndent)
            maybe unit setDisplay (s^.tagDisplay)
            case t of
              "nodoc" -> subDoc (Pure "Not documented:":subs)
              _ -> subDoc subs
            styleEnd
            getl (activeStyle.l'2.tagPrefix)
          activeStyle =~ (l'1 %- False) . (l'2.tagPrefix %- pref)
          styleStart
        doc' (Pure t) = do
          st <- getl showState
          (_,stl') <- getl activeStyle
          let isRaw = fromMaybe False (stl'^.tagIsRawText)
          case st of
            EndP b -> do
              tell (if b then "\n\n" else "\n")
              column =- 0
              showState =- BeginP
            InP wasRaw -> do
              if isRaw && wasRaw then unit else tellText " "
            _ -> unit
          styleStart
          tellText t
          showState =- InP isRaw
        subDoc docs = traverse_ doc' docs

        boolSt b k = maybe unit (\x -> if x then k else unit) b
        styleStart = do
          (isSet,TagStyle (cf,cb) bl bo u it p _ _ _) <- getl activeStyle
          unless isSet $ do
            activeStyle.l'1 =- True
            maybe unit setDisplay bl
            indent
            maybe unit (\pre -> tellText pre >> (activeStyle.l'2.tagPrefix =- Nothing)) p
            tell (restoreDefaultColors trm)
            maybe unit (tell . setForegroundColor trm) cf
            maybe unit (tell . setBackgroundColor trm) cb
            boolSt bo (tell $ setBold trm True)
            boolSt u (tell $ setUnderlined trm True)
            boolSt it (tell $ setItalic trm True)
            
        styleEnd = do
          (isSet,TagStyle (fg,bg) bl bo u it _ _ _ _) <- getl activeStyle
          when isSet $ do
            maybe unit endDisplay bl
            boolSt bo (tell $ setBold trm False)
            boolSt u (tell $ setUnderlined trm False)
            boolSt it (tell $ setItalic trm False)
            maybe unit (const (tell $ restoreDefaultColors trm)) (fg+bg)

        indent = do
          st <- getl showState
          pref <- getl (activeStyle.l'2.tagPrefix)
          case st of
            BeginP -> getl indentDepth >>= \n -> tellText (take (n - maybe 0 length pref) (repeat ' '))
            _ -> unit
        withIndent n = go
          where go "" = ""
                go ('\n':t) = '\n' : (take n (repeat ' ') + go t)
                go (c:t) = c : go t

        bType b st = b || case st of EndP x -> x ; _ -> False
        setDisplay (Block b) = getl showState >>= \st -> do
          case st of
            BeginP -> unit
            _ -> showState =- EndP (bType b st)
        setDisplay _ = unit
        endDisplay (Block b) = showState =~ \st' -> EndP (bType b st')
        endDisplay _ = unit

pretty :: Documented t => t -> String
pretty t = docString DummyTerminal defaultStyle (document t)

-- | A class for all types that can be formatted to a string
class Show a => FormatArg a where
  argClass :: a -> Char
  showFormat :: a -> String
  showFormat = show
-- | A base class for the 'format' function
class FormatType a where
  format' :: String -> String -> a
instance (FormatArg a,FormatType r) => FormatType (a -> r) where
  format' x ('%':c:t) a | c == argClass a = format' (reverse (showFormat a)+x) t
                        | otherwise = error "Invalid format argument type"
  format' x (c:t) a = format' (c:x) t a
  format' _ [] _ = error "Unused argument in format"
instance FormatType String where
  format' x t = reverse x+t
instance FormatArg Int where argClass _ = 'd'
instance FormatArg Float where argClass _ = 'f'
instance FormatArg Double where argClass _ = 'f'
instance FormatArg String where argClass _ = 's'; showFormat = id
instance FormatArg PortNumber where argClass _ = 'p'

-- | A function that mimics sprintf-style formatting for Haskell
format :: FormatType r => String -> r
format = format' ""
