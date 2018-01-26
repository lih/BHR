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

showDoc = "{section {title Formatted Query} {p {em Usage:} show PATH PATTERN} {p Show the function at PATH according to the given pattern}}"
showCmd = withDoc showDoc . fill False $ do
  path <- (nbhspace >> ((several "{}" >> getSession wd) <+? absPath ""))
          <+? getSession wd
  pat <- option' (docTag' "pattern" [Pure "default"])
         (nbhspace >> ((docAtom <*= guard . has t'Join) <+? map (docTag' "pattern" . pure . Pure) dirArg))
  withMountain $ let ctx = fold $ c'list $ localContext^??atMs path in do
    let params (n,v) = let Join p = composing (uncurry insert) [
                             (["type"],Pure $ document (exprType (v^.leafVal))),
                             (["name"],Pure $ Pure $ identName n),
                             (["doc"],Pure $ v^.leafDoc),
                             (["impl"],Pure $ Pure $ showImpl (v^.leafVal)),
                             (["strictness"],Pure $ document (snd $ exprStrictness $ v^.leafVal))
                             ] zero
                       in p
    withStyle $ withPatterns $ serveStrLn (docString ?terminal ?style (document (map (\v -> fromMaybe (nodoc (format "Unmatched pattern %s" (show pat))) (evalDocWithPatterns ?patterns (params v) pat)) ctx)))
    
patternCmd = withDoc "{section {title Define Formatting Patterns} {p {em Usage:} pattern PATH = PATTERN} {p Defines a new query pattern accessible with \\{pattern PATH\\}}}" . fill False $ do
  ph:pt <- many1' (nbhspace >> dirArg <*= guard . (/="="))
  between nbhspace nbhspace (several "=")
  pat <- docLine "pat" []
  liftIO $ runAtomic ?sessionState (patterns.at ph.l'Just (Join zero).at pt =- Just (Pure pat))
