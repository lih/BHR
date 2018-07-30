{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo, QuasiQuotes #-}
module Curly.Session.Commands.Context where

import Curly.Core
import Curly.Core.Annotated
import Curly.Core.Documentation
import Curly.Core.Library
import Curly.Core.Parser
import Curly.Session.Commands.Common
import Curly.UI
import Curly.UI.Options hiding (nbsp,spc)
import IO.Filesystem
import Language.Format hiding (space)
import System.Posix.Files
import Control.DeepSeq (rnf)

buildCmd,cleanCmd,metaCmd,reloadCmd,fixCmd :: Interactive Command

cleanDoc = [q_string| 
{title Clean Cache}
{p {em Usage:} clean {em OR} clean PATH}
{p Removes all cache files under PATH.}
|]
cleanCmd = withDoc cleanDoc $ False <$ do
  base <- option' [] (nbhspace >> absPath [])
  let isPrefixOf p p' = take (length p) p' == p
  liftIO (do sequence_ [clean c | (p,Source _ _ c) <- ?curlyPlex^.mounts, base`isPrefixOf`p]
             sequence_ [clean c | (p,Resource _ c) <- ?curlyPlex^.mounts, base`isPrefixOf`p])
  where clean c = do
          x <- getFile c
          forl_ (descendant.fileAttrs.relPath) x $ \p -> case c+p of
            p' | drop (length p' - 4) p' == ".cyl"  -> trylog unit (removeLink p')
            _ -> unit
metaDoc = [q_string|
{title Show Metadata} 
{p Show the metadata associated with the given path}
|]
metaCmd = withDoc metaDoc $ fill False $ withMountain $ do
  path <- many' (nbhspace >> dirArg)
  let mod = ?mountain >>= \fl -> mapF (\m -> ModDir (m^.ascList)) (Join (fl^.flLibrary.metadata.iso (\(Metadata m) -> m) Metadata))
  serveStrLn $ maybe "" showMetaDir (mod^?atMs path)

reloadDoc = [q_string|
{title Reload All Files}

{p Reload the context}
|]
reloadCmd = withDoc reloadDoc (False <$ reloadMountain)

buildDoc = [q_string|
{title Build Path} 
{p {em Usage:} build {em OR} build PATH}
{p Build and cache all libraries under PATH and their dependencies, so that
   subsequent builds can be performed offline.}
|]
buildCmd = withDoc buildDoc $ fill False $ withMountain $ do
  base <- option' [] (nbhspace >> absPath "")
  let cache (_,e) = liftIO $ rnf (anonymous (e^.leafVal))^.from thunk
  traverse_ cache (localContext^??atMs base.traverse)

fixDoc = [q_string|
{title Fix Error} 
{p {em Usage:} fix {em OR} fix ERROR_NUMBER}
{p Run an editing session at the location of the last error numbered 
   ERROR_NUMBER.}
{p If the error number is unspecified, it defaults to the first error.}
|]
fixCmd = withDoc fixDoc $ False <$ do
  i <- option' 1 (nbhspace >> number)
  (s,ws) <- getSession warnings
  case (s,drop (i-1) ws) of
    (Just s, Warning _ (l,c) _:_) -> liftIOWarn $ editSource s (l,c) reloadMountain
    _ -> serveStrLn $ format "Error: there is no error #%d" i
