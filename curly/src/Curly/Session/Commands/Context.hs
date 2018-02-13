{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo #-}
module Curly.Session.Commands.Context where

import Curly.Core
import Curly.Core.Library
import Curly.UI
import Curly.UI.Options hiding (nbsp,spc)
import Curly.Core.Parser
import Language.Format hiding (space)
import IO.Filesystem
import System.Posix.Files
import Curly.Session.Commands.Common

cleanCmd,metaCmd,reloadCmd,fixCmd :: Interactive Command

cleanDoc = "{section {title Clean Cache} {p {em Usage:} clean {em OR} clean PATH} {p Removes all cache files under PATH.}}"
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
metaDoc = "{section {title Show Metadata} Show the metadata associated with the given path}"
metaCmd = withDoc metaDoc $ fill False $ withMountain $ do
  path <- many' (nbhspace >> dirArg)
  let mod = ?mountain >>= \fl -> mapF (\m -> ModDir (m^.ascList)) (Join (fl^.flLibrary.metadata.iso (\(Metadata m) -> m) Metadata))
  serveStrLn $ maybe "" showMetaDir (mod^?atMs path)

reloadDoc = "{section {title Reload All Files} Reload the context}"
reloadCmd = withDoc reloadDoc (False <$ reloadMountain)

fixDoc = "{section {title Fix Error} Runs an editing session for fixing the last error.}"
fixCmd = withDoc fixDoc $ False <$ do
  i <- option' 1 (nbhspace >> number)
  (s,ws) <- getSession warnings
  case (s,drop (i-1) ws) of
    (Just s, Warning _ (l,c) _:_) -> liftIOWarn $ editSource s (l,c) reloadMountain
    _ -> serveStrLn $ format "Error: there is no error #%d" i
