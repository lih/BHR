{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo #-}
module Curly.Session.Commands.Run where

import Curly.Core
import Curly.Core.Annotated
import Curly.Core.Library
import Curly.UI.Options hiding (nbsp,spc)
import Curly.Core.Parser
import Curly.Core.Documentation
import Curly.System
import Language.Format hiding (space)
import Curly.Session.Commands.Common
import Curly.UI

runCmd :: Interactive Command
runDoc = unlines [
  "{section {title Run Curly Expression (Experimental)}",
  "  {p {em Usage:} run EXPRESSION {em OR} run PATH}",
  "  {p Runs a Curly expression.}}"
  ]
runCmd = withDoc runDoc $ False <$ do
  nbsp >> (runExpr <+? runSym)
  where runExpr = do
          e <- between (single '(') (single ')') (accessorExpr HorizSpaces)
          l <- getSession this
          liftIOWarn $ doRun (anonymous (exprIn l e))
        runSym = do
          p <- absPath ""
          withMountain $ case localContext^?atMs p of
            Just (Pure (_,v)) -> liftIOWarn $ doRun (anonymous (v^.leafVal))
            _ -> guardWarn Sev_Error ("The path "+showSymPath p+" doesn't point to a runnable function") False
        doRun ex = do
          logLine Verbose $ "Running expression "+pretty (semantic ex :: Expression GlobalID (Symbol GlobalID))
          runIt <- jitExpr (?curlyPlex^.jitContext) ex
          runIt
    
