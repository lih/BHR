{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo #-}
module Curly.Session.Commands.Run where

import Curly.Core
import Curly.Core.Annotated
import Curly.Core.Library
import Curly.UI.Options hiding (nbsp,spc)
import Curly.Core.Parser
import Curly.System
import Language.Format hiding (space)
import Curly.Session.Commands.Common

runCmd :: Interactive Command
runDoc = unlines [
  "{section {title Run Curly Expression (Experimental)}",
  "  {p {em Usage:} run EXPRESSION}",
  "  {p Runs a Curly expression.}}"
  ]
runCmd = withDoc runDoc $ False <$ do
  e <- nbsp >> accessorExpr hspace
  l <- getSession this
  liftIOWarn $ do
    let ex = anonymous (exprIn l e)
    logLine Verbose $ "Running expression "+pretty (semantic ex :: Expression GlobalID (Symbol GlobalID))
    runIt <- jitExpr (?curlyPlex^.jitContext) ex
    runIt
    
