{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo, QuasiQuotes #-}
module Curly.Session.Commands.Navigation where

import Curly.Core
import Curly.Core.Library
import Curly.UI
import Curly.Core.Parser
import Data.IORef 
import Language.Format
import Curly.Session.Commands.Common
import Curly.Core.Documentation
import Curly.Style (setupTermFromEnv)
import Curly.UI.Options (symPath)

lsCmd,wdCmd,cdCmd,treeCmd :: Interactive Command
lsDoc = [q_string|
{title List Directory}
{p {em Usage:} ls {em OR} ls PATH}
{p List the contents of the working directory, or those of the relative path given on the command-line.}
|]
lsCmd = withDoc lsDoc $ False <$ do
  p <- hspace >> absPath ""
  withMountain $ serveStrLn $
    if has (atMs p.t'Pure) localContext
    then "Error: "+showPath p+" is a function"
    else let ModDir mod = fold (c'list $ localContext^??atMs p.t'Join)
         in intercalate " " (c'set $ fromKList (fst<$>mod))

treeDoc = [q_string|
{title Show Directory Tree}
{p {em Usage:} tree {em OR} tree PATH}
{p Recursively list the contents of the working directory, or those of the relative path given on the command-line.}
|]
treeCmd = withDoc treeDoc $ False <$ do
  p <- hspace >> absPath ""
  term <- liftIO setupTermFromEnv
  withStyle $ withMountain $ serveStrLn . docString term ?style . document . map fst . Join . fold . c'list $ (localContext^??atMs p.t'Join)

cdDoc = [q_string|
{title Change Directory}
{p {em Usage:} cd {em OR} cd PATH}
{p Set the working directory to the path given on the command-line.
   Paths are taken relative to the working directory before changing.}
|]
cdCmd = withDoc cdDoc (fill False $ withargs <+? noarg)
  where noarg = liftIO (modifyIORef ?sessionState (wd %- []))
        withargs = nbhspace >> do
          newpath <- absPath ""
          withMountain $ do
            let m = c'list (localContext^??atMs newpath)
            liftIOWarn $ if nonempty (fold $ c'list (m^??each.t'Join))
                         then modifyIORef ?sessionState (wd %- newpath)
                         else serveStrLn $ if has (each.t'Pure) m
                                           then "Error: "+showPath newpath+" is a function"
                                           else "Error: there is nothing under "+showPath newpath
wdDoc = [q_string|{title Print Working Directory} {p Prints the working directory}|]
wdCmd = withDoc wdDoc $ False <$ (getSession wd >>= serveStrLn . showPath)
