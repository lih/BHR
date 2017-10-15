{-# LANGUAGE ImplicitParams, StandaloneDeriving #-}
module Definitive (
  module Definitive.Base,
  module Data.Containers,
  module Data.Containers.Sequence,
  trace,tracing,trace2,mtrace,debug,

  cli
  ) where

import Definitive.Base 
import System.Environment (getArgs)
import Data.Containers
import Data.Containers.Sequence

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
