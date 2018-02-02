{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo #-}
module Curly.Session.Commands(
  -- * Sessions
  SessionState,wd,this,style,patterns,
  withSessionState,withStyle,getSession,

  -- * Commands
  KeyOps(..),Interactive,Command,commands,commandNames,

  -- * Parsers
  interactiveSession,

  -- * Utilities
  subPath,dirArg,colorNames,serveStrLn,serveString
  ) where

import Curly.Core
import Curly.Core.Annotated
import Curly.Core.Documentation
import Curly.Core.Library
import Curly.Core.Security
import Curly.UI
import Curly.Core.Parser
import Curly.Style
import Data.Char (isSpace)
import Language.Format hiding (space)
import Control.Exception (toException)
import Curly.Session.Commands.Common
import Curly.Session.Commands.Navigation
import Curly.Session.Commands.Query
import Curly.Session.Commands.Context
import Curly.Session.Commands.Style
import Curly.Session.Commands.Key
import Curly.Session.Commands.Repository
import Curly.Session.Commands.Run
import Curly.Session.Commands.VCS

commands :: Interactive [(String,[(String,Command)])]
commands = [
  ("Navigation",[
      ("cd",cdCmd),
      ("ls",lsCmd),
      ("edit",editCmd),
      ("tree",treeCmd),
      ("wd",wdCmd)]),

  ("Documentation",[
      ("help",helpCmd),
      ("meta",metaCmd),
      ("style",styleCmd),
      ("pattern",patternCmd),
      ("show",showCmd),
      ("compareTypes",compareTypesCmd),
      ("instances",showInstancesCmd)]),
  
  ("Control",[
      ("key",keyCmd),
      ("clean",cleanCmd),
      ("reload",reloadCmd),
      ("configure",configCmd),
      ("run",runCmd),
      ("fix",fixCmd),
      ("repository",repoCmd),
      ("vcs",vcsCmd),
      ("quit",quitCmd),
      ("kill-server",killCmd)])]

commandNames :: [String]
commandNames = let
  ?sessionState = undefined
  ?targetParams = undefined
  ?curlyPlex = undefined
  ?curlyConfig = undefined
  ?serve = undefined
  ?edit = undefined
  ?killServer = undefined
  ?quitSession = undefined
  ?access = undefined
  ?clientOps = undefined
  ?subSession = undefined
  ?terminal = undefined
  in map fst $ foldMap snd commands

quitCmd,helpCmd,configCmd,killCmd,compareTypesCmd,showInstancesCmd :: Interactive Command

compareTypesDoc = "{section {title Compare Types} Compares the types of two expressions}"
compareTypesCmd = withDoc compareTypesDoc $ False <$ do
  let exprT = map exprType . optimized
  nbsp
  shapeCmp <- (True <$ several "shape") <+? (False <$ several "constraints")
  nbsp
  a <- exprT =<< tom AnySpaces
  nbsp
  b <- exprT =<< tom HorizSpaces
  serveStrLn $ if shapeCmp then show (compare a b) else show (compareConstrainedness a b)

showInstancesDoc = "{section {title Show Instances} Shows all the instances of the current execution context}"
showInstancesCmd = withDoc showInstancesDoc $ False <$ do
  imps <- lift $ getl $ l'library.implicits
  when (envLogLevel >= Debug) $ serveStrLn $ format "Valid: %s" (show $ isValidInstanceMap imps)
  for_ (imps^.ascList) $ \((n,t),(_,e)) -> do
    serveStrLn $ format "Instance %s (%s): %s\n%s" (pretty n) (show t) (show (e^.leafType)) (pretty (map fst $ semantic (e^.leafVal) :: Expression GlobalID GlobalID))

quitDoc = "{section {title Quit} Quit the program}"
quitCmd = withDoc quitDoc $ liftIO ?quitSession >> return True

subTag t = t'Join.docNodeSubs.traverse.sat (isTag t)
  where isTag x (Join (DocTag t _ _)) = t==x
        isTag _ _ = False

helpDoc = "{section {title Show Help} Show the help for the given function, or all of them.}"
helpCmd = withDoc helpDoc $ False <$ do
  args <- many' (nbhspace >> dirArg)
  term <- liftIO setupTermFromEnv
  liftIOWarn $ case args of
    [] -> withStyle $ do
      let docTag a s = Join (DocTag a [] s)
          lis = commands <&> \(h,cmds) ->
            docTag "p" . (:) (docTag "title" [Pure (h+":")]) . pure . docTag "ul"
            $ cmds <&> \(c,(d,_)) ->
            let sub = d^?subTag "section".subTag "title".t'Join.docNodeSubs
            in docTag "li" [Pure (c+":"),docTag "em" (head sub)]
      serveStrLn $ docString term ?style (docTag "doc" (Pure "Here are the available commands (enter 'help <cmd>' to show specific sections) :":lis))
    (cmd:_) -> case foldMap snd commands^.at cmd of
      Just (d,_) -> withStyle (serveStrLn $ docString term ?style d)
      _  -> serveStrLn $ "Error: "+cmd+": no such command."

configDoc = unlines [
  "{section {title Instance Configuration} ",
  "  {p {em Usage:} configure <selector>} ",
  "  {p Open a configuration file for edition.",
  "  {line If many configurations are available, the first one whose name matches the selector is edited.}}}"
  ]
configCmd = withDoc configDoc $ False <$ do
  sel <- option' 0 ((nbhspace >> many1' (noneOf "\n")) >*> number)
  case drop sel (curlyFiles ?curlyConfig) of
    file:_ | ?access >= Admin -> liftIOWarn (readBytes file >>= ?edit "" (0,0) >>= maybe unit (writeBytes file))
           | otherwise -> serveStrLn "Error: You are not allowed to access the instance configuration"
    [] -> serveStrLn $ format "Error: Couldn't find configuration file number '%d'" sel

killDoc = unlines [
  "{section {title Kill Instance Server}",
  "  {p Kills the server for the current instance, if there is one.}}"
  ]
killCmd = withDoc killDoc $ True <$ if ?access >= Admin then liftIOWarn (?quitSession >> ?killServer)
                                    else serveStrLn "Error: you need admin access to kill a server"

interactiveSession :: Interactive (IO () -> OpParser IO ())
interactiveSession ack = while sessionLine
  where sessionLine = do
          (ws,ln) <- intercept $ option' Nothing (map Just line)
          case ln of
            Just end -> liftIO ack >> return (not end)
            Nothing -> do
              err <- many' (satisfy (/='\n')) <* eol
              liftIOWarn $ when (any (not . isSpace) err) $ throw (toException $ CurlyParserException Nothing ws)
              liftIOLog ack
              return True
        line = withMountain $ do
          (ws,ln) <- listen $ muteOnSuccess $ option' Nothing (Just <$> withSessionLib curlyLine)
          case ln of
            Just _ -> return False
            Nothing -> guard (empty ws) >> cmdLine
        parseCmd = hspace >> do
          e <- optimized =<< accessorExpr HorizSpaces
          lookingAt (hspace >> eol)
          serveStrLn (showImpl e)
          return False
        cmdLine = do
          s <- remaining
          cmd <- hspace >> many1' (satisfy (\c -> not (isSpace c || c=='\'')))
          maybe (runStreamState (put s) >> parseCmd) snd (foldMap snd commands^.at cmd) <* hspace <* (eol+eoi)

