{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, DeriveGeneric #-}
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
import qualified Haste.Binary as JS hiding (get)
import qualified Prelude as P
import qualified Data.Array.Unboxed as Arr

instance Semigroup JS.JSString where (+) = JSS.append
instance Monoid JS.JSString where zero = JSS.empty
instance Sequence JS.JSString where splitAt = JSS.splitAt
instance StackSymbol JS.JSString where
  atomClass c = case c JSS.! 0 of
    '{' | JSS.length c==1 -> Open Brace
    ',' | JSS.length c==2 && c JSS.! 1 == '{' -> Open (Splice CloseConstant)
    '$' | JSS.length c==2 && c JSS.! 1 == '{' -> Open (Splice CloseExec)
    '}' | JSS.length c==1 -> Close
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

newtype FSIO a = FSIO (ReaderT JSFS JS.CIO a)
  deriving (Functor,SemiApplicative,Unit,Applicative,MonadIO)
instance P.Functor FSIO where fmap = map
instance P.Applicative FSIO where (<*>) = (<*>)
instance P.Monad FSIO where return = return ; (>>=) = (>>=)
instance JS.MonadIO FSIO where liftIO = liftIO
instance Monad FSIO where join = coerceJoin FSIO
instance JS.MonadConc FSIO where
  liftCIO x = FSIO (lift x)
  fork (FSIO rx) = FSIO (rx & from readerT %~ \r x -> JS.fork (r x))
instance MonadSubIO FSIO FSIO where liftSubIO = id

instance Serializable [Word8] Char where encode _ c = ListBuilder (fromIntegral (fromEnum c):)
instance Format [Word8] Char where datum = datum <&> \x -> toEnum (fromEnum (x::Word8))
instance Format [Word8] (ReadImpl  FSIO String String) where datum = return (ReadImpl getString)
instance Format [Word8] (ReadImpl  FSIO String [Word8]) where datum = return (ReadImpl getBytes)
instance Format [Word8] (WriteImpl FSIO String String) where datum = return (WriteImpl setString)
instance Format [Word8] (WriteImpl FSIO String [Word8]) where datum = return (WriteImpl setBytes)

runComment c = unit
toWordList :: JS.JSString -> [Word8]
toWordList = map (fromIntegral . fromEnum) . toString 

type ErrorMessage = String

collectConc :: (Monad m, JS.MonadConc m) => ((a -> IO ()) -> (err -> IO ()) -> IO ()) -> m (err :+: a)
collectConc k = do
  v <- JS.newEmptyMVar
  JS.liftCIO $ JS.liftIO $ k (\x -> JS.concurrent $ JS.putMVar v (Right x)) (\err -> JS.concurrent $ JS.putMVar v (Left err))
  JS.readMVar v

fsSchema :: JS.JSAny -> IO ()
fsSchema = JS.ffi "(CaPriCon.initFS)"

newtype JSFS = JSFS JS.JSAny
instance JS.ToAny JSFS where
  toAny (JSFS fs) = fs
  listToAny l = JS.listToAny (map (\(JSFS x) -> x) l)
instance JS.FromAny JSFS where
  fromAny x = return (JSFS x)
  listFromAny x = map JSFS <$> JS.listFromAny x
newFS_impl :: JS.JSString -> (JSFS -> IO ()) -> (JS.JSAny -> IO ()) -> IO ()
newFS_impl = JS.ffi "(CaPriCon.newFS)" fsSchema

newFS :: JS.JSString -> JS.CIO JSFS
newFS db = do
  ret <- collectConc (newFS_impl db)
  case ret of
    Left _ -> error $ "Couldn't open database backend for " + toString db
    Right r -> return r

getFSItem_impl :: JSFS -> JS.JSString -> (JS.JSString -> IO ()) -> (JS.JSAny -> IO ()) -> IO ()
getFSItem_impl = JS.ffi "(CaPriCon.getFSItem)"

getFSItem :: JS.JSString -> FSIO (JS.JSAny :+: JS.JSString)
getFSItem file = FSIO ask >>= \fs -> collectConc (getFSItem_impl fs file)

setFSItem_impl :: JSFS -> JS.JSString -> JS.JSString -> (JS.JSAny -> IO ()) -> (JS.JSAny -> IO ()) -> IO ()
setFSItem_impl = JS.ffi "(CaPriCon.setFSItem)"

setFSItem :: JS.JSString -> JS.JSString -> FSIO ()
setFSItem file dat = void $ FSIO ask >>= \fs -> collectConc (setFSItem_impl fs file dat)

getString :: String -> FSIO (ErrorMessage :+: String)
getString fileS = do
  let file = fromString fileS :: JS.JSString
  mres <- getFSItem file
  case mres of
    Right res -> return (Right $ toString (res :: JS.JSString))
    Left _ -> do
      here <- JS.getLocationHref
        
      let url = JSS.replace here (JSS.regex "/[^/]*$" "") ("/"+file)
      res <- collectConc (JS.ffi "(CaPriCon.ajaxGetString)" url)
      case res of
        Left x -> liftIO (JS.fromAny x) <&> \(n,msg) -> Left . toString $ "HTTP error "+fromString (show (n::Int))+" while retrieving "+url+": "+msg
        Right val -> Right (toString (val :: JS.JSString)) <$ setFSItem file val
getBytes :: String -> FSIO (ErrorMessage :+: [Word8])
getBytes fileS = do
  let file = fromString fileS :: JS.JSString
  mres <- getFSItem file
  case mres of
    Right res -> return (Right $ toWordList (res :: JS.JSString))
    Left _ -> do
      here <- JS.getLocationHref
        
      let url = JSS.replace here (JSS.regex "/[^/]*$" "") ("/"+file)
      res <- collectConc (JS.ffi "(CaPriCon.ajaxGetString)" url)
      case res of
        Left x -> liftIO (JS.fromAny x) <&> \(n,msg) -> Left . toString $ "HTTP error "+fromString (show (n::Int))+" while retrieving "+url+": "+msg
        Right val -> Right (toWordList val) <$ setFSItem file val
setString :: String -> String -> FSIO ()
setString f v = setFSItem (fromString f) (fromString v :: JS.JSString)
setBytes :: String -> [Word8] -> FSIO ()
setBytes f v = setString f (map (toEnum . fromIntegral) v)

type WiQEEState = StackState (COCState String) String (COCBuiltin FSIO String) (COCValue FSIO String)
runWordsState :: [String] -> WiQEEState -> FSIO (WiQEEState,String)
runWordsState ws st = ($st) $ from (stateT.concatT) $^ do
  foldr (\w tl -> do
            x <- runExtraState (getl endState)
            unless x $ do execSymbol runCOCBuiltin runComment w; tl) unit ws
  out <- runExtraState (outputText <~ \x -> (id,x))
  return (out "")

runWithFS :: JS.JSString -> FSIO a -> JS.CIO a
runWithFS fsname (FSIO r) = newFS fsname >>= r^..readerT

hasteDict = cocDict ("0.12.2.1-js" :: String) getString getBytes setString setBytes

main :: IO ()
main = do
  -- JS.ffi "console.log" ("hasteMain called" :: JS.JSString) :: IO ()
  Just msg <- JS.lookupAny capriconObject "event.data"
  (req,reqID,stateID,code) <- JS.fromAny msg
  sts <- JS.get capriconObject "states"
  JS.concurrent $ runWithFS "CaPriCon" $ do
    st <- case stateID of
      0 -> return (defaultState hasteDict (COCState False [] zero id))
      _ -> liftIO $ map JS.fromOpaque $ JS.index sts (stateID-1) 
    case req :: Int of
      -- run a block of code, and return a handle to a new state
      0 -> do
        (st',_) <- runWordsState (map toString $ stringWords (code :: JS.JSString)) st
        id <- appendState capriconObject st'
        postMessage (reqID :: Int,id)
  
      -- run a block of code, and return its output, discarding the new state
      1 -> do
        (_,out) <- runWordsState (map toString $ stringWords (code :: JS.JSString)) st
        postMessage (reqID :: Int,fromString out :: JS.JSString)
  
      _ -> error "Unhandled request type"
  
appendState :: MonadIO m => JS.JSAny -> a -> m Int
appendState obj x = liftIO $ JS.ffi "(function (o,a) { o.states.push(a); return o.states.length; })" obj (JS.toOpaque x)

postMessage :: (MonadIO m,JS.ToAny a) => a -> m ()
postMessage msg = liftIO $ JS.ffi "(function (m) { postMessage(m); })" (JS.toAny msg)

capriconObject :: JS.JSAny
capriconObject = JS.constant "CaPriCon"
    
  -- maybe unit JS.focus =<< JS.elemById "content-scroll"
  -- JS.wait 200

  -- let withSubElem root cl = JS.withElemsQS root ('.':cl) . traverse_
  --     withSubElems _ [] k = k []
  --     withSubElems root (h:t) k = withSubElem root h $ \h' -> withSubElems root t $ \t' -> k (h':t')
  
  -- prelude <- JS.withElem "capricon-prelude" (\e -> JS.getProp e "textContent")
  -- (initState,_) <- runWordsState (map fromString $ stringWords prelude) (defaultState hasteDict (COCState False [] zero id))

  -- roots <- JS.elemsByQS JS.documentBody ".capricon-steps, code.capricon"
  -- Just console <- JS.elemById "capricon-console"

  -- (\k -> foldr k (\_ _ -> unit) roots initState "") $ \root next state pref -> do
  --   isCode <- JS.hasClass root "capricon"

  --   if isCode
  --     then do
  --     p <- JS.getProp root "textContent"
  --     next state (pref+p+" pop ")
  --     else do
  --       JS.wait 10
    
  --       root' <- cloneNode root
  --       JS.toggleClass root' "capricon-frame"
  --       rootChildren <- JS.getChildren root'
  --       rootTitle <- JS.newElem "h3" <*= \head -> JS.appendChild head =<< JS.newTextElem "CaPriCon Console"
  --       closeBtn <- JS.newElem "button" <*= \but -> JS.appendChild but =<< JS.newTextElem "Close"
  --       JS.appendChild rootTitle closeBtn
  --       JS.appendChild console root'
  --       JS.setChildren root' (rootTitle:rootChildren)

  --       withSubElems root ["capricon-trigger"] $ \[trig] -> void $ do
  --         withSubElems root' ["capricon-input"] $ \[inpCons] -> void $ do
  --           let toggleActive = do
  --                 JS.toggleClass root' "active"
  --                 JS.focus inpCons
  --           JS.onEvent closeBtn JS.Click (const toggleActive)
  --           JS.onEvent trig JS.Click $ \_ -> toggleActive
            
  --       withSubElems root ["capricon-input"] $ \[inpMain] -> do
  --         withSubElems root' ["capricon-input","capricon-output"] $ \[inp,out] -> do
  --           JS.withElemsQS root' ".capricon-context" $ \case
  --             [con] -> do
  --               context <- JS.getProp con "textContent"
  --               let text = pref+" "+context
  --               -- JS.alert ("Running "+fromString text)
  --               (state',_) <- runWordsState (stringWords text) state
  --               let onEnter x = \case
  --                     JS.KeyData 13 False False False False -> x
  --                     _ -> return ()
  --                   runCode inp = do
  --                     Just v <- JS.getValue inp
  --                     (_,x) <- runWordsState (stringWords v) state'
  --                     JS.setProp out "textContent" (toString x)
  --                     return v
  --               JS.onEvent inp JS.KeyPress $ onEnter $ void $ runCode inp
  --               JS.onEvent inpMain JS.KeyPress $ onEnter $ do
  --                 v <- runCode inpMain
  --                 JS.setClass root' "active" True
  --                 JS.focus inp
  --                 JS.setProp inp "value" v
  --               JS.setClass inpMain "ready" True
  --               next state' ""

-- cloneNode :: MonadIO m => JS.Elem -> m JS.Elem
-- cloneNode x = liftIO $ JS.ffi "(function (n) { return n.cloneNode(true); })" x
