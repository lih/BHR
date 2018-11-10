module Main where

import Definitive
import Algebra.Monad.Concatenative
import Control.Concurrent (threadDelay)

stringWords :: String -> [String]
stringWords = map fromString . fromBlank
  where fromBlank (c:t) | c `elem` [' ', '\t', '\r', '\n'] = fromBlank t
                        | c == '"' = fromQuote id t
                        | otherwise = fromWChar (c:) t
        fromBlank "" = []
        fromQuote k ('"':t) = ('"':k "\""):fromBlank t
        fromQuote k ('\\':c:t) = fromQuote (k.(qChar c:)) t
          where qChar 'n' = '\n' ; qChar 't' = '\t' ; qChar x = x
        fromQuote k (c:t) = fromQuote (k.(c:)) t
        fromQuote k "" = ['"':k "\""]
        fromWChar k (c:t) | c `elem` [' ', '\t', '\r', '\n'] = k "":fromBlank t
                          | otherwise = fromWChar (k.(c:)) t
        fromWChar k "" = [k ""]

dict = fromAList [("wait",StackExtra $ Opaque (Wait 100)),
                  ("quit",StackExtra $ Opaque Quit)]

data LogosBuiltin = Wait Int | Quit
data LogosState = LogosState {
  _running :: Bool
  }
running :: Lens' LogosState Bool
running = lens _running (\x y -> x { _running = y })

runLogos (Wait n) = do
  liftIO $ threadDelay n
runLogos Quit = runExtraState $ do running =- False

main = do
  text <- readHString stdin
  let go (w:ws) = do
        liftIO $ putStrLn $ "Running : "+w
        execSymbol runLogos (\_ -> unit) w
        r <- runExtraState $ getl running
        if r then go ws else unit
      go [] = unit
  (go (stringWords text)^..stateT.concatT) (defaultState dict (LogosState True))
        
  putStrLn "Hello from Logos !"
