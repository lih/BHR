{-# LANGUAGE CPP #-}
module Main where

import Definitive
import Algebra.Monad.Concatenative
import System.IO (hIsTerminalDevice)
import System.Environment (getArgs)
import Console.Readline (readline,addHistory,setCompletionEntryFunction)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.IORef
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))
import CaPriCon.Run

nativeDict = cocDict VERSION_capricon readString writeString

main = do
  isTerm <- hIsTerminalDevice stdin
  libdir <- getXdgDirectory XdgData "capricon"
  symList <- newIORef (keys nativeDict)
  let getAll = unsafeInterleaveIO $ do
        ln <- readline "CaPriCon> "
        lns <- getAll
        case ln of
          Just x -> do addHistory x; return $ x + " .\n" + lns
          Nothing -> putStr "\n" >> return ""
  setCompletionEntryFunction $ Just $ \line -> do
    sl <- readIORef symList
    case reverse (words (line+"?")) of
      "?":_ -> return sl
      wp:_ -> let wps = length wp-1; wp' = init wp in return [w | w <- sl, take wps w==wp']
      _ -> return []
  str <- stringWords <$> if isTerm then getAll else readHString stdin
  args <- (foldMap (\x -> [libdir</>x,x]) <$> getArgs) >>= map (stringWords . fold) . traverse (try (return []) . readString)
  execS (foldr (\sym mr -> do
                   execSymbol runCOCBuiltin outputComment sym
                   (hasQuit,out) <- runExtraState (liftA2 (,) (getl endState) (getl outputText) <* (outputText =- id))
                   d <- runDictState get
                   lift (writeIORef symList (keys d))
                   lift (putStr (out ""))
                   unless hasQuit mr
               ) unit (args+str)^..concatT) (defaultState nativeDict (COCState False [] zero id))
