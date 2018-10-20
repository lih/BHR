{-# LANGUAGE ImplicitParams, StandaloneDeriving, CPP #-}
module Definitive (
  module Definitive.Base,
  module Data.Containers,
#ifndef __HASTE__
  module Data.Containers.Sequence,
#endif
  trace,tracing,trace2,mtrace,debug,

  cli
  ) where

import Definitive.Base 
import System.Environment (getArgs)
import Data.Containers
#ifndef __HASTE__
import Data.Containers.Sequence
#endif

trace :: String -> a -> a
trace s x = (putStrLn s^.thunk)`seq`x
tracing :: (a -> String) -> a -> a
tracing f a = trace (f a) a
trace2 :: String -> String -> a -> a
trace2 b a x = trace b (x`seq`trace a x)
mtrace :: Unit f => String -> f ()
mtrace str = trace str (pure ())
debug :: Show a => a -> a
debug x = trace (show x) x

cli :: String -> (( ?cliargs :: [String], ?programName :: String ) => IO a) -> IO a
cli name main = getArgs >>= \a -> let ?programName = name ; ?cliargs = a in main

deriving instance Show (f (g a)) => Show ((f:.:g) a)
