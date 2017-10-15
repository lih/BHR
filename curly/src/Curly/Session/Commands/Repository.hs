{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo #-}
module Curly.Session.Commands.Repository where

import Curly.Core
import Curly.Core.Library
import Curly.Core.Parser
import Curly.Core.Security
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
          repos <- readIORef repositories
          for_ repos $ \r -> serveStrLn $ format "  * %s" (show r)
        repoLibs = (opKeyword "contents" >>) $ do
          t <- option' defaultTemplate (docLine "template" [])
          liftIOWarn $ do
            ls <- availableLibs
            for_ [(l,d) | (l,Just d) <- map (second (`showTemplate`t)) ls] $ \(l,d) -> do
              serveStrLn $ format "  * %s (%s)" d (show l)
        repoAdd = do
          opKeyword "add"
          guardWarn "Error: you must have admin access to add repositories" (?access>=Admin)
          r <- nbsp >> many' (noneOf "\n") >*> readable
          liftIOWarn $ modifyIORef repositories (touch r)
        repoBrowse = do
          opKeyword "browse"
          guardWarn "Error: you must have almighty acces to browse new libraries" (?access>=Almighty)
          nbsp
          sel <- (\d libs -> [x | x@(_,m) <- libs, nonempty (showTemplate m d)]) <$> (docAtom <*= guard . has t'Join)
                 <+? (\i -> select ((==i) . fst)) <$> (dirArg >*> readable)
          ls <- liftIO (map sel availableLibs)
          case ls of
            [(i,_)] -> ?subSession [(Nothing,Mount [] (Library i))]
            _ -> guardWarn "Error: the pattern should select a single library to browse" False
