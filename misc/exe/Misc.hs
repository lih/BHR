module Main where

import Definitive
import Curly.Core.Security
import System.Environment (getArgs)

main = do
  [file] <- getArgs
  a <- readString file
  k <- genPrivateKey
  sa <- signValue k a
  case extractSignedBy (publicKey k) sa of
    Just a' -> print a >> print a'
    Nothing -> putStrLn "Invalid signature !"
