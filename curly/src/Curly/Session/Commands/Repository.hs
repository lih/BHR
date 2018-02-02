{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo #-}
module Curly.Session.Commands.Repository where

import Curly.Core
import Curly.Core.Library
import Curly.Core.Parser
import Curly.Core.Security
import Curly.Core.VCS
import Curly.Session.Commands.Common
import Curly.UI.Options (CurlyOpt(..),InputSource(..))
import Data.IORef 
import Language.Format hiding (space)

repoCmd :: Interactive Command
repoDoc = unlines ["{section {title Manage Repositories} "
                  ," {p {em Usage:} repository list {em OR} repository contents {em OR} repository add <repo> "
                  ,"   {ln {em \"    OR\"} repository browse (<library-id>|<search-pattern>)}} "
                  ," {p Lists known repositories, the libraries therein or adds a new one.}}"]
repoCmd = withDoc repoDoc $ False <$ (nbsp >>) (repoList <+? repoLibs <+? repoAdd <+? repoBrowse)
  where repoList = (opKeyword "list" >>) $ liftIOWarn $ do
          VCSB_Native repos _ _ <- readIORef libraryVCS
          for_ repos $ \r -> serveStrLn $ format "  * %s" r
        repoLibs = (opKeyword "contents" >>) $ do
          t <- option' defaultTemplate (docLine "template" [])
          liftIOWarn $ withStyle $ withPatterns $ do
            ls <- availableLibs
            for_ [(l,d) | (l,Just d) <- map (second (\d -> showTemplate ?terminal ?style ?patterns d t)) ls] $ \(l,d) -> do
              serveStrLn $ format "%s %s" (show l) d
        repoAdd = do
          opKeyword "add"
          guardWarn Sev_Error "you must have admin access to add repositories" (?access>=Admin)
          r <- nbsp >> many' (noneOf "\n") >*> readable
          liftIOWarn $ modifyIORef libraryVCS (+ r)
        repoBrowse = do
          opKeyword "browse"
          guardWarn Sev_Error "you must have almighty acces to browse new libraries" (?access>=Almighty)
          nbsp
          sel <- (\d -> do
                     libs <- availableLibs
                     return [x | (x,m) <- libs, nonempty (showDummyTemplate m d)]) <$> (docAtom <*= guard . has t'Join)
                 <+? (\i -> return [i]) <$> (dirArg >*> readable)
          ls <- liftIO sel
          case ls of
            [i] -> ?subSession [(Nothing,Mount [] (Library i))]
            _ -> guardWarn Sev_Error "the pattern should select a single library to browse" False
