{-# LANGUAGE CPP #-}
module Main where

import Definitive
import Language.Format
import Algebra.Monad.Concatenative
import System.IO (hIsTerminalDevice)
import System.Environment (getArgs)
import Console.Readline (readline,addHistory,setCompletionEntryFunction)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.IORef
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))
import CaPriCon.Run
import Data.CaPriCon (ListBuilder(..))

instance Serializable [Word8] Char where encode _ c = ListBuilder (fromIntegral (fromEnum c):)
instance Format [Word8] Char where datum = datum <&> \x -> toEnum (fromEnum (x::Word8))
instance Format [Word8] (ReadImpl IO String String) where datum = return (ReadImpl f_readString)
instance Format [Word8] (ReadImpl IO String [Word8]) where datum = return (ReadImpl f_readBytes)
instance Format [Word8] (WriteImpl IO String String) where datum = return (WriteImpl writeString)
instance Format [Word8] (WriteImpl IO String [Word8]) where datum = return (WriteImpl (\x -> writeBytes x . pack))

f_readString = (\x -> catch (return . Left . show) (Right<$>readString x))
f_readBytes = (\x -> catch (return . Left . show) (Right . unpack<$>readBytes x))

nativeDict = cocDict VERSION_capricon f_readString f_readBytes writeString (\x -> writeBytes x . pack)

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
                   execSymbol runCOCBuiltin outputComment (atomClass sym)
                   (hasQuit,out) <- runExtraState (liftA2 (,) (getl endState) (getl outputText) <* (outputText =- id))
                   d <- runDictState get
                   lift (writeIORef symList (keys d))
                   lift (putStr (out ""))
                   unless hasQuit mr
               ) unit (args+str)^..concatT) (defaultState nativeDict (COCState False [] zero id))
