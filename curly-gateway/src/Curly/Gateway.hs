{-# LANGUAGE DeriveGeneric, TypeFamilies, ScopedTypeVariables, PatternSynonyms, ViewPatterns, ExistentialQuantification, TypeOperators #-}
module Main(
  main
  ) where

import Definitive
import Language.Format hiding (hspace,nbhspace)
import System.Environment (getArgs)
import Curly.Core.Documentation
import Curly.DHT.Kademlia
import Curly.Core.Security
import Curly.Core.Library
import Curly.Core.VCS
import IO.Network.Socket
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import System.Console.GetOpt (OptDescr(..),ArgDescr(..),usageInfo,getOpt,ArgOrder(..))
import qualified Prelude as P
import Curly.Core (B64Chunk(..))

instance Functor OptDescr where map = P.fmap

instance DHTIndex (VCKey DHTKey)

data DHTKey = DataKey ValID
            | NodeKey String
            deriving (Eq,Ord,Generic)
instance Serializable DHTKey ; instance Format DHTKey
type Key = VCKey DHTKey

newtype ValID = ValID Hash
              deriving (Eq,Ord,Show,Generic)
instance Serializable ValID; instance Format ValID

data Val = PartialVal [ValID]
         | DataVal Bytes
         deriving (Show,Generic)
instance Serializable Val ; instance Format Val
instance Eq Val where a==b = compare a b==EQ
instance Ord Val where compare = comparing serialize
instance DHTValue Val

newtype DHT_VC a = DHT_VC { runDHT_VC :: IO a }
                 deriving (Functor,Unit,SemiApplicative,Applicative)
instance Monad DHT_VC where join = coerceJoin DHT_VC
instance MonadIO DHT_VC where liftIO = DHT_VC
instance MonadVC DHT_VC (DHTInstance Key Val) where
  vcStore st k = insertMP st (map2 (const undefined) k)
  vcLoad st k = lookupMP st (map2 (const undefined) k)
  runVC (DHT_VC a) = a

valID :: Val -> ValID
valID = ValID . hashData . serialize

parMap :: Traversable t => (a -> IO b) -> t a -> IO (t b)
parMap k ta = for ta $ \a -> do
  v <- newEmptyMVar
  forkIO (k a >>= putMVar v)
  return (readMVar v^.thunk)

insertMPBytes :: DHTInstance Key Val -> Bytes -> IO ValID
insertMPBytes dht b
  | bytesSize b<=256 = let v = DataVal b ; cid = valID v in insertDHT dht (OtherKey (DataKey cid)) v >> return cid
  | otherwise = let parts = mapAccum_ (\n b -> swap $ splitAt n b) (take 8 $ repeat ((bytesSize b + 7) `div` 8)) b 
                in parMap (insertMPBytes dht) parts >>= \ps -> let v = PartialVal ps ; cid = valID v
                                                               in cid `seq` insertDHT dht (OtherKey (DataKey cid)) v >> return cid

lookupMPBytes :: DHTInstance Key Val -> Key -> IO (Maybe Bytes)
lookupMPBytes dht k = do
  x <- lookupDHT dht k
  case x of
    Just (DataVal b,_) -> return (Just b)
    Just (PartialVal ks,_) -> do
      parts <- parMap (lookupMPBytes dht . OtherKey . DataKey) ks
      return (fold <$> sequence parts)
    Nothing -> return Nothing

lookupMP :: (Format a,MonadIO m) => DHTInstance Key Val -> (WithResponse a -> Key) -> m (Maybe a)
lookupMP dht fk = liftIO $ lookupMPBytes dht (fk WithResponse) <&> (>>= matches Just datum)
insertMP :: (Serializable a,MonadIO m) => DHTInstance Key Val -> (WithResponse a -> Key) -> a -> m ()
insertMP dht fk a = liftIO $ insertMPBytes dht (serialize a) >>= \cid -> insertDHT dht (fk WithResponse) (PartialVal [cid])

isValidAssoc (OtherKey (DataKey h)) v = valID v == h
isValidAssoc _ _ = True

data DHTAction = ServeHTTP PortNumber | ServeNative PortNumber | Help
data DHTMode = DHTMode {
  dhtAction :: DHTAction,
  dhtBackend :: IO VCSBackend
  }

extendedBackend :: Parser String (IO VCSBackend)
extendedBackend = map (foldr1 (+)) . sequence <$> sepBy1' (dhtBackend <+? map return readable) nbspace
  where dhtBackend = do
          several "dht:"
          server <- option' Nothing $ map Just $ do
            several "//"
            root <- many1' (noneOf ": \t\n")
            port <- option' 25464 (single ':' >> number)
            single '/' + lookingAt (eol + eoi)
            return (root,port)
          myPort <- number
          return $ do
            dht <- dhtInstance myPort
            case server of
              Nothing -> return (VCSB_Native ("dht:"+show myPort) dht (\(DHT_VC m) -> m))
              Just (srv,port) -> do
                res <- joinDHT dht (DHTNode srv port (OtherKey (NodeKey ("curly-vc "+show port))))
                case res of
                  JoinSucces -> putStrLn $ "Successfully joined network node "+srv+":"+show port
                  _ -> error "Couldnt't reach root node"
                return (VCSB_Native ("dht://"+srv+":"+show port+"/") dht (\(DHT_VC m) -> m))

dhtOpts = [
  Option "h" ["help"] (NoArg (\x -> x { dhtAction = Help })) "Shows the help menu",
  Option "s" ["store"] (ReqArg (\b x -> x { dhtBackend = fromMaybe (pure dummyBackend) (matches Just extendedBackend b) }) "BACKEND")
  "Set the backend storage",
  Option ""  ["http"] (OptArg (\mp x -> x { dhtAction = ServeHTTP (fromMaybe 8080 (map read mp)) }) "PORT") "Serve a HTTP repository",
  Option ""  ["native"] (OptArg (\mp x -> x { dhtAction = ServeNative (fromMaybe 5402 (map read mp)) }) "PORT") "Serve a native Curly repository"
  ]

dhtInstance dhtport = newDHTInstance (fromIntegral dhtport) (OtherKey (NodeKey ("curly-vc "+show dhtport))) isValidAssoc

hspace = skipMany' (char (`elem`" \t"))
nbhspace = skipMany1' (char (`elem`" \t"))
char p = datum <*= guard . p
charOut p = char (\c -> not (c`elem`p))
kw str = traverse_ char (map (==) str)

data HTTPMessageType = GET String | PUT String | Response
                     deriving Show
data HTTPMessage = HTTPMessage {
  messageType :: HTTPMessageType,
  messageHeaders :: Map String String,
  messageBody :: Bytes
  }
                 deriving Show
instance Serializable HTTPMessageType where
  encode (GET uri) = foldMap encode ("GET " + uri + " HTTP/1.1\r\n")
  encode (PUT uri) = foldMap encode ("PUT " + uri + " HTTP/1.1\r\n")
  encode Response = foldMap encode ("HTTP/1.1 200 OK\r\n")
instance Format HTTPMessageType where
  datum = req <+? resp
    where req = do
            t <- (GET <$ kw "GET") <+? (PUT <$ kw "PUT")
            uri <- nbhspace >> many1' (charOut " \r\n")
            skipMany' (charOut "\r\n")
            kw "\r\n"
            return (t uri)
          resp = kw "HTTP/1.1" >> skipMany' (charOut "\r\n") >> kw "\r\n" >> return Response
  
instance Serializable HTTPMessage where
  encode (HTTPMessage t hdrs body) =
    encode t
    + foldMap (\(n,v) -> foldMap encode (n+": "+v+"\r\n")) (hdrs^.ascList)
    + foldMap encode "\r\n"
    + body^.bytesBuilder
instance Format HTTPMessage where
  datum = do
    t <- datum
    hdrs <- map fromAList $ many' $ do
      name <- many1' (charOut "\r\n :")
      val <- kw ":" >> hspace >> (fold <$> sepBy1' (many1' (charOut " \t\r\n")) nbhspace)
      hspace >> kw "\r\n"
      return (name,val)
    kw "\r\n"
    body <- case t of
      GET _ -> return zero
      _ -> runStreamState (id <~ swap . splitAt (maybe 0 read (hdrs^.at "Content-Length")))
    return (HTTPMessage t hdrs body)
  
main = do
  (args,_,_) <- getOpt (ReturnInOrder (const id)) dhtOpts <$> getArgs
  let mode = compose args (DHTMode Help (pure dummyBackend))
  case dhtAction mode of
    Help -> putStrLn (usageInfo "curly-vc" dhtOpts)
    ServeNative port -> do
      sock <- listenOn port
      backend <- dhtBackend mode
      forever $ do
        (h,_) <- accept sock
        void $ forkIO $ runConnection_ True h $ forever $ vcServer backend
    ServeHTTP port -> do
      sock <- listenOn port
      backend <- dhtBackend mode
      forever $ do
        (h,_) <- accept sock
        void $ forkIO $ runConnection_ True h $ do
          receive >>= \msg -> do
            (msgMethod,msgURI) <- case messageType msg of
              GET uri -> return ("GET",uri)
              PUT uri -> return ("PUT",uri)
              _ -> zero
            let msgKeyName = select (/='/') msgURI
                msgKey :: Maybe (VCKey ())
                msgKey = matches Just ((readable <&> \(B64Chunk b) -> b^..chunk) >*> datum) msgKeyName
                getKey, putKey :: Format a => (WithResponse a -> VCKey ()) -> ParserT Bytes IO ()
                getKey k = vcbLoad backend k >>= \res -> do
                  case res of
                    Just body -> send (HTTPMessage Response (fromAList [("connection","close")]) (serialize body))
                    Nothing -> zero
                putKey k = case matches Just datum (messageBody msg) of
                  Just v -> vcbStore backend k v >> send (HTTPMessage Response (fromAList [("connection","close")]) zero)
                  Nothing -> zero
            case (msgMethod,msgKey) of
              ("GET",Just (LibraryKey l _))       -> getKey (LibraryKey l)
              ("GET",Just (AdditionalKey l s _))  -> getKey (AdditionalKey l s)
              ("GET",Just (BranchesKey pub _))    -> getKey (BranchesKey pub)
              ("GET",Just (CommitKey h _))        -> getKey (CommitKey h)
              ("PUT",Just (LibraryKey l _))       -> putKey (LibraryKey l)
              ("PUT",Just (AdditionalKey l s _))  -> putKey (AdditionalKey l s)
              ("PUT",Just (BranchesKey pub _))    -> putKey (BranchesKey pub)
              ("PUT",Just (CommitKey h _))        -> putKey (CommitKey h)
              _ -> zero
