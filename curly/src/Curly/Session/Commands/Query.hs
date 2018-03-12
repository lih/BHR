{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo #-}
module Curly.Session.Commands.Query where

import Curly.Core
import Curly.Core.Annotated
import Curly.Core.Documentation
import Curly.Core.Library
import Curly.UI
import Curly.Core.Parser
import Curly.Style
import Language.Format hiding (space)
import Curly.Session.Commands.Common

editCmd,showCmd,patternCmd :: Interactive Command

data VerboseVar = VerboseVar GlobalID (Maybe Int)
instance Documented VerboseVar where
  document (VerboseVar v n) = Pure $ pretty v+maybe "" (\x -> "["+show x+"]") n
showImpl v | envLogLevel>=Verbose = pretty (map withSym (semantic v) :: Expression GlobalID VerboseVar)
           | otherwise = pretty (map fst (semantic v) :: Expression GlobalID GlobalID)
  where withSym (s,Pure (Argument n)) = VerboseVar s (Just n)
        withSym (s,_) = VerboseVar s Nothing
          
rangeFile :: Traversal' SourceRange String
rangeFile k (SourceRange (Just s) a b) = k s <&> \s' -> SourceRange (Just s') a b
rangeFile _ x = pure x

viewCmd doc onExpr onPath showV = withDoc doc . fill False $ (several "'s" >> viewSym) <+? viewPath
  where viewPath = nbsp >> do
          path <- absPath ""
          withMountain $ case localContext^?atMs path of
            Just (Pure (_,v)) -> liftIOWarn $ showV path v
            _ -> onPath path
                 <+? serveStrLn ("Error: "+showPath path+" isn't a function.")
        viewSym = (nbsp >>) . (<+? onExpr) $ do
          n <- dirArg
          lookingAt (eoi+eol)
          l <- getSession this
          liftIOWarn $ case l^.symbols.at n of
            Just s -> showV [] s
            _ -> serveStrLn $ "Error: "+n+": no such symbol."

editDoc = unlines [
  "{section {title Edit Function}"
  ,"{p {em Usage:} edit PATH}"
  ,"{p Start an editing session for the function at PATH.}}"
  ]
editCmd = viewCmd editDoc zero onPath $ \path (by leafPos -> r) -> case r of
  SourceRange (Just f) (_,l,c) _ -> editSource f (l,c) reloadMountain
  _ -> serveStrLn $ "No source position available for "+showPath path 
  where onPath p = withMountain $ do
          case ?mountain^?atMs p.t'Pure.flLibrary.symbols.traverse.leafPos.rangeFile of
            Just s -> liftIOWarn $ editSource s (0,0) reloadMountain
            _ -> zero

showExprDefault pat n v = do
  let Join params = composing (uncurry insert) [
        (["flavor"],Pure $ Pure "Expression"),
        (["name"],Pure $ Pure n),
        (["type"],Pure $ document (exprType v)),
        (["raw-type"],Pure $ Pure $ show (exprType v & \(Type e) -> e)),
        (["impl"],Pure $ Pure $ showImpl v),
        (["strictness"],Pure $ document (snd $ exprStrictness v))
        ] zero
  serveStrLn (docString ?terminal ?style (fromMaybe (nodoc $ "Cannot show pattern "+showRawDoc pat)
                                          (evalDocWithPatterns ?patterns params pat)))

showDoc = unlines [
  "{section {title Formatted Query} {p {em Usage:} show (PATH|\\\\(EXPR\\\\)) [PATTERN]}",
  "  {p Show information about functions under PATH, or an ad-hoc expression}",
  "  {p The pattern will default to '\\{call show-default\\}' if left unspecified.}}"
  ]
showCmd = withDoc showDoc . fill False $ do
  epath <- map Right (nbhspace >> between (single '(') (single ')') (withParsedString (expr AnySpaces)))
           <+? map Left ((nbhspace >> ((several "{}" >> getSession wd) <+? absPath ""))
                         <+? (lookingAt (hspace >> eol) >> getSession wd))
  pat <- option' (docTag' "call" [Pure "show-default"])
         (nbhspace >> ((docAtom <*= guard . has t'Join) <+? map (docTag' "call" . pure . Pure) dirArg))
  withMountain $ withPatterns $ withStyle $ case epath of
    Left path -> let ctx = fold $ c'list $ localContext^??atMs path in do
      let params (n,v) = let Join p = composing (uncurry insert) [
                               (["flavor"],Pure $ Pure "Symbol"),
                               (["type"],Pure $ document (exprType (v^.leafVal))),
                               (["name"],Pure $ Pure $ identName n),
                               (["doc"],Pure $ v^.leafDoc),
                               (["impl"],Pure $ Pure $ showImpl (v^.leafVal)),
                               (["strictness"],Pure $ document (snd $ exprStrictness $ v^.leafVal))
                               ] zero
                         in p
          l'void :: Lens Void Void a a
          l'void = lens (\_ -> undefined :: Void) (\x _ -> x)
          applyFilter (Pure v) = case evalDocWithPatterns ?patterns (params v) pat of
            Just d -> Pure d
            Nothing -> Join (ModDir [])
          applyFilter (Join (ModDir l)) = Join (ModDir (select
                                                        (has (l'2.(t'Pure.l'void .+ t'Join.i'ModDir.traverse.l'void)))
                                                        (map2 applyFilter l)))
      serveStrLn (docString ?terminal ?style (document (applyFilter ctx)))

    Right (n,e) -> do
      v <- optExprIn <$> getSession this <*> pure e
      showExprDefault pat n v
patternDoc = unlines [
  "{section {title Define Formatting Patterns} {p {em Usage:} pattern NAME ARG... = PATTERN {em OR} pattern NAME}",
  "  {p Defines a new query pattern accessible with \\{pattern PATTERN PARAM...\\}}",
  "  {p If you only specify the pattern name, its current definition will be printed instead.}}"]
patternCmd = withDoc patternDoc . fill False $ do
  ph:pt <- many1' (nbhspace >> dirArg <*= guard . (/="="))
  let setPat = do
        between nbhspace nbhspace (several "=")
        pat <- docLine "pat" []
        liftIO $ runAtomic ?sessionState (patterns.at ph =- Just (pt,pat))
      showPat = do
        pat <- liftIO $ runAtomic ?sessionState (getl (patterns.at ph))
        case pat of
          Just (_,pat) -> serveStrLn (format "pattern %s%s = %s" ph (foldMap (" "+) pt) (showRawDoc pat))
          Nothing -> serveStrLn (format "The pattern %s doesn't exist." ph)
  setPat <+? showPat
