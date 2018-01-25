{-# LANGUAGE TypeFamilies #-}
module Curly.Core.Documentation(
  -- * The Documentation format
  DocNode(..),Documentation,Documented(..),
  docNodeAttrs,docNodeSubs,
  docTag,docTag',nodoc,mkDoc,docAtom,docLine,
  DocParams,
  evalDoc,evalDocWithPatterns,
  -- * Rendering documentation
  -- ** Styles
  TagStyle(..),TermColor(..),TagDisplay(..),Style,defaultStyle,
  tagColor,tagDisplay,tagIsBold,tagIsUnderlined,tagIndent,tagPrefix,tagIsItalic,
  -- ** Rendering
  Terminal(..),DummyTerminal(..),docString,pretty
  ) where

import Definitive
import Language.Format

-- | A documentation node (similar to a HTML node, but simpler)
data DocNode a = DocTag String [(String,String)] [a]
               deriving (Eq,Ord,Show,Generic)
instance Serializable a => Serializable (DocNode a)
instance Format a => Format (DocNode a)
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

type DocParams = Forest (Map String) Documentation
evalDocWithPatterns :: DocParams -> DocParams -> Documentation -> Maybe Documentation
evalDocWithPatterns pats vars = eval
  where eval (Pure x) = return (Pure x)
        eval (Join (DocTag "$" [] xs)) = do
          xs' <- traverse eval xs
          path <- for xs' $ \x -> x^?t'Pure
          Join vars^?at path.t'Just.t'Pure
        eval (Join (DocTag "pattern" [] xs)) = do
          xs' <- traverse eval xs
          path <- for xs' $ \x -> x^?t'Pure
          pat <- Join pats^?at path.t'Just.t'Pure
          eval pat
        eval (Join (DocTag "or" [] xs)) = foldMap eval xs
        eval (Join (DocTag t as xs)) = Join . DocTag t as <$> traverse eval xs
evalDoc :: DocParams -> Documentation -> Maybe Documentation
evalDoc = evalDocWithPatterns zero

nodoc msg = Join (DocTag "nodoc" [] [Pure msg])
mkDoc d = Join . DocTag "doc" [] $ fromMaybe [] $ matches Just (between spc spc (sepBy' docAtom spc)) d
spc :: (ParseStream c s, ParseToken c, TokenPayload c ~ Char,Monad m) => ParserT s m ()
spc = skipMany' (oneOf " \t\n")
docAtom :: (ParseStream c s, ParseToken c, TokenPayload c ~ Char,Monad m) => ParserT s m Documentation
docAtom = tag <+? txt
  where letter p = token >>= \c -> case c of
          '\\' -> token
          _ | (c`isKeyIn`reserved) || not (p c) -> zero
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
        txt = (Pure <$> many1' (letter (/='"'))) <+? strSplice
          where strSplice = between (single '"') (single '"') $ do
                  h <- option' id ((:) . Left <$> many1' strChar)
                  t <- many' (map Left (many1' strChar) <+? map Right tag)
                  return $ case h t of
                    [Left s] -> Pure s
                    l -> Join $ DocTag "splice" [] (map (Pure <|> id) l)
                strChar = satisfy (\x -> not (x`elem`"\"{\\"))
                          <+? single '\\' >> token
docLine :: (ParseToken c, ParseStream c s, TokenPayload c ~ Char, Monad m)
           => String -> [(String,String)] -> ParserT s m Documentation
docLine n as = Join . DocTag n as <$> many1' (skipMany' (oneOf " \t") >> docAtom)

data ShowState = BeginP | InP | EndP Bool
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
  _tagIndent :: Maybe Int
  }
instance Semigroup TagStyle where
  TagStyle c bl bo u it p i + TagStyle c' bl' bo' u' it' p' i' = TagStyle (c'+c) (bl'+bl) (bo'+bo) (u'+u) (it'+it) (p'*p+p+p') (i'+i)
instance Monoid TagStyle where
  zero = TagStyle zero zero zero zero zero zero zero

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

defaultStyle :: Style
defaultStyle = fromAList [
  ("p",isBl zero),
  ("title",(isB . isBl) zero),
  ("nodoc",zero & tagColor.l'1 %- Just (ColorNumber 67)),
  ("section",isBl zero),
  ("em",isB zero),
  ("ul",compose [set tagDisplay (Just (Block True)), set tagIndent (Just 2)] zero),
  ("li",((tagDisplay %- Just (Block False)) . (tagPrefix %- Just "- ")) zero),
  ("modDir",set tagPrefix (Just "* ") zero),
  ("ln",set tagDisplay (Just (Block False)) zero),
  ("sub",set tagIndent (Just 2) zero)
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
  
docString :: Terminal trm => trm -> Style -> Documentation -> String
docString trm stl d = getId ((doc' d^..i'RWST) ((),(BeginP,zero,0))) & \(_,_,t) -> t
  where addStyles s s' = (s+s') & set tagPrefix (s'^.tagPrefix + s^.tagPrefix) 
        tagStl t as = foldl' addStyles zero [stl^.at c.folded | ("class",c) <- (("class",t):as)]
        doc' (Join (DocTag t as subs)) = do
          l'2.l'2 =~ compose [tagDisplay %- Nothing,tagIndent %- Nothing]
          pref <- saving l'2 $ saving l'3 $ do
            let tstl = tagStl t as
            l'2 =~ \(_,s) -> (False,(s + tstl))
            s <- getl (l'2.l'2)
            maybe unit (\i -> l'3 =~ maybe id ((+) . length) (tstl^.tagPrefix) . (+i)) (s^.tagIndent)
            maybe unit setDisplay (s^.tagDisplay)
            case t of
              "nodoc" -> doc' (Pure "Not documented.")
              _ -> subDoc subs
            styleEnd
            getl (l'2.l'2.tagPrefix)
          l'2 =~ (l'1 %- False) . (l'2.tagPrefix %- pref)
          styleStart
        doc' (Pure t) = do
          st <- getl l'1
          case st of
            EndP b -> do
              tell (if b then "\n\n" else "\n")
              l'1 =- BeginP
            InP -> tell " "
            _ -> unit
          styleStart
          ind <- getl l'3
          tell (withIndent ind t)
          l'1 =- InP
        subDoc docs = traverse_ doc' docs

        boolSt b k = maybe unit (\x -> if x then k else unit) b
        styleStart = do
          (isSet,TagStyle (cf,cb) bl bo u it p _) <- getl l'2
          unless isSet $ do
            l'2.l'1 =- True
            maybe unit setDisplay bl
            indent
            maybe unit (\pre -> tell pre >> (l'2.l'2.tagPrefix =- Nothing)) p
            tell (restoreDefaultColors trm)
            maybe unit (tell . setForegroundColor trm) cf
            maybe unit (tell . setBackgroundColor trm) cb
            boolSt bo (tell $ setBold trm True)
            boolSt u (tell $ setUnderlined trm True)
            boolSt it (tell $ setItalic trm True)
            
        styleEnd = do
          (isSet,TagStyle (fg,bg) bl bo u it _ _) <- getl l'2
          when isSet $ do
            maybe unit endDisplay bl
            boolSt bo (tell $ setBold trm False)
            boolSt u (tell $ setUnderlined trm False)
            boolSt it (tell $ setItalic trm False)
            maybe unit (const (tell $ restoreDefaultColors trm)) (fg+bg)

        indent = do
          st <- getl l'1
          pref <- getl (l'2.l'2.tagPrefix)
          case st of
            BeginP -> getl l'3 >>= \n -> tell (take (n - maybe 0 length pref) (repeat ' '))
            _ -> unit
        withIndent n = go
          where go "" = ""
                go ('\n':t) = '\n' : (take n (repeat ' ') + go t)
                go (c:t) = c : go t

        bType b st = b || case st of EndP x -> x ; _ -> False
        setDisplay (Block b) = getl l'1 >>= \st -> do
          case st of
            BeginP -> unit
            _ -> l'1 =- EndP (bType b st)
        setDisplay _ = unit
        endDisplay (Block b) = l'1 =~ \st' -> EndP (bType b st')
        endDisplay _ = unit

pretty :: Documented t => t -> String
pretty t = docString DummyTerminal defaultStyle (document t)
