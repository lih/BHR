{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Main where

import Definitive
import Language.Format
import Algebra.Monad.Concatenative
import System.IO (openFile,hIsTerminalDevice,IOMode(..),hClose)
import System.Environment (getArgs,lookupEnv)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.IORef
import Data.CaPriCon
import CaPriCon.Run
import System.FilePath (dropFileName,(</>))
import qualified Haste.Foreign as JS
import qualified Haste as JS
import qualified Haste.DOM as JS
import qualified Haste.Events as JS
import qualified Haste.Concurrent as JS
import qualified Haste.Ajax as JS
import qualified Haste.JSString as JSS
import qualified Haste.LocalStorage as JS
import qualified Haste.Binary as JS
import qualified Prelude as P
import qualified Data.Array.Unboxed as Arr

instance Semigroup JS.JSString where (+) = JSS.append
instance Monoid JS.JSString where zero = JSS.empty
instance Sequence JS.JSString where splitAt = JSS.splitAt
instance StackSymbol JS.JSString where
  atomClass c = case c JSS.! 0 of
    '{' | JSS.length c==1 -> OpenBrace
    '}' | JSS.length c==1 -> CloseBrace
    '\'' -> Quoted (drop 1 c)
    '"' -> Quoted (take (JSS.length c-2) (drop 1 c))
    ':' -> Comment (drop 1 c)
    _ -> maybe (Other c) Number $ matches Just readable (toString c)
instance IsCapriconString JS.JSString where
  toString = JSS.unpack

instance Functor JS.CIO where map = P.fmap
instance SemiApplicative JS.CIO where (<*>) = (P.<*>)
instance Unit JS.CIO where pure = P.return
instance Applicative JS.CIO
instance Monad JS.CIO where join = (P.>>=id)
instance MonadIO JS.CIO where liftIO = JS.liftIO
instance MonadSubIO JS.CIO JS.CIO where liftSubIO = id

instance Serializable Word8 ([Word8] -> [Word8]) [Word8] Char where encode _ c = (fromIntegral (fromEnum c):)
instance Format Word8 ([Word8] -> [Word8]) [Word8] Char where datum = datum <&> \x -> toEnum (fromEnum (x::Word8))
instance Format Word8 ([Word8] -> [Word8]) [Word8] (ReadImpl  JS.CIO String String) where datum = return (ReadImpl getString)
instance Format Word8 ([Word8] -> [Word8]) [Word8] (ReadImpl  JS.CIO String [Word8]) where datum = return (ReadImpl getBytes)
instance Format Word8 ([Word8] -> [Word8]) [Word8] (WriteImpl JS.CIO String String) where datum = return (WriteImpl setString)
instance Format Word8 ([Word8] -> [Word8]) [Word8] (WriteImpl JS.CIO String [Word8]) where datum = return (WriteImpl setBytes)

runComment c = unit
toWordList :: JS.JSString -> [Word8]
toWordList = map (fromIntegral . fromEnum) . toString 

getString :: String -> JS.CIO (Maybe String)
getString file = do
  mres <- liftIO $ JS.getItem (fromString file)
  case mres of
    Right res -> return (Just $ toString (res :: JS.JSString))
    Left _ -> do
      here <- toString <$> JS.getLocationHref
        
      let url = fromString (dropFileName here</>file)
      res <- JS.ajax JS.GET url
      case res of
        Left JS.NetworkError -> fill Nothing $ JS.alert $ "Network error while retrieving "+url
        Left (JS.HttpError n msg) -> fill Nothing $ JS.alert $ "HTTP error "+fromString (show n)+": "+msg
        Right val -> map Just $ liftIO $ JS.setItem (fromString file) val >> return (toString (val :: JS.JSString))
getBytes :: String -> JS.CIO (Maybe [Word8])
getBytes file = do
  mres <- liftIO $ JS.getItem (fromString file)
  case mres of
    Right res -> return (Just $ toWordList (res :: JS.JSString))
    Left _ -> do
      here <- toString <$> JS.getLocationHref
        
      let url = fromString (dropFileName here</>file)
      res <- JS.ajax JS.GET url
      case res of
        Left JS.NetworkError -> fill Nothing $ JS.alert $ "Network error while retrieving "+url
        Left (JS.HttpError n msg) -> fill Nothing $ JS.alert $ "HTTP error "+fromString (show n)+": "+msg
        Right val -> map Just $ liftIO $ JS.setItem (fromString file) val >> return (toWordList val)
setString :: String -> String -> JS.CIO ()
setString f v = liftIO $ JS.setItem (fromString f) (fromString v :: JS.JSString)
setBytes :: String -> [Word8] -> JS.CIO ()
setBytes f v = setString f (map (toEnum . fromIntegral) v)

hasteDict :: COCDict JS.CIO String
hasteDict = cocDict ("0.8-js" :: String) getString getBytes setString setBytes

main :: IO ()
main = JS.concurrent $ void $ do
  let runWordsState ws st = ($st) $ from (stateT.concatT) $^ do
        foldr (\w tl -> do
                  x <- runExtraState (getl endState)
                  unless x $ do execSymbol runCOCBuiltin runComment w; tl) unit ws
        out <- runExtraState (outputText <~ \x -> (id,x))
        return (out "")
      withSubElem root cl = JS.withElemsQS root ('.':cl) . traverse_
      withSubElems _ [] k = k []
      withSubElems root (h:t) k = withSubElem root h $ \h' -> withSubElems root t $ \t' -> k (h':t')
  
  prelude <- JS.withElem "capricon-prelude" (\e -> JS.getProp e "textContent")
  (initState,_) <- runWordsState (map fromString $ stringWords prelude) (defaultState hasteDict (COCState False [] zero id))

  roots <- JS.elemsByClass "capricon-steps"
  Just console <- JS.elemById "capricon-console"

  (\k -> foldr k (const unit) roots initState) $ \root next state -> do
    JS.wait 10

    root' <- cloneNode root
    JS.toggleClass root' "capricon-frame"
    rootChildren <- JS.getChildren root'
    rootTitle <- JS.newElem "h3" <*= \head -> JS.appendChild head =<< JS.newTextElem "CaPriCon Console"
    closeBtn <- JS.newElem "button" <*= \but -> JS.appendChild but =<< JS.newTextElem "Close"
    JS.appendChild rootTitle closeBtn
    JS.appendChild console root'
    JS.setChildren root' (rootTitle:rootChildren)

    withSubElems root ["capricon-trigger"] $ \[trig] -> void $ do
      withSubElems root' ["capricon-input"] $ \[inp] -> void $ do
        let toggleActive = do
              JS.toggleClass root' "active"
              JS.focus inp
        JS.onEvent closeBtn JS.Click (const toggleActive)
        JS.onEvent trig JS.Click $ \_ -> toggleActive
    withSubElems root' ["capricon-input","capricon-output"] $ \[inp,out] -> do
      JS.withElemsQS root' ".capricon-context" $ \case
        [con] -> do
          context <- JS.getProp con "textContent"
          (state',_) <- runWordsState (stringWords (fromString context)) state
          JS.onEvent inp JS.KeyPress $ \case
            JS.KeyData 13 False False False False -> do
              Just v <- JS.getValue inp
              (_,x) <- runWordsState (stringWords v) state'
              JS.setProp out "textContent" (toString x)
            _ -> unit
          next state'

cloneNode :: MonadIO m => JS.Elem -> m JS.Elem
cloneNode x = liftIO $ JS.ffi "(function (n) { return n.cloneNode(true); })" x
