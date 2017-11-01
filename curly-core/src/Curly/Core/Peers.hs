module Curly.Core.Peers where

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Curly.Core
import Curly.Core.Library
import Data.IORef 
import GHC.Conc (threadDelay)
import IO.Network.Socket
import Language.Format
import System.IO (hSetBuffering,BufferMode(..))

data PeerPacket = DeclareInstance String (WithResponse (Either String PortNumber))
                | RedeclareInstance String PortNumber (WithResponse Bool)
                | DeclareSupply
                | AskInstance String (WithResponse (Either String PortNumber))
                | AskInstances (WithResponse [String])
                | AskLibrary LibraryID (WithResponse Bytes)
                | ListLibraries (WithResponse [(LibraryID,Metadata)])
                deriving Generic
instance Serializable PortNumber where
  encode = encode . c'int . fromIntegral
instance Format PortNumber where
  datum = fromIntegral . c'int <$> datum
instance Serializable PeerPacket
instance Format PeerPacket where
  datum = defaultDatum
          <+? ListLibraries WithResponse <$ sequence_ [datum >>= guard . (==c) . debug | c <- "libraries"]
          <+? (\x -> AskLibrary x WithResponse) <$> ((fromMaybe zero . matches Just (many' datum)<$>runStreamState (id <~ swap . splitAt 43)) >*> readable)

data LocalSupply = LocalSupply (IO [(LibraryID,Metadata)]) (LibraryID -> IO (Maybe Bytes))
data SupplyAnswer = SupplyLibraries [(LibraryID,Metadata)]
                  | SupplyLibrary Bytes
                  deriving Generic
instance Serializable SupplyAnswer ; instance Format SupplyAnswer

processInstances :: IORef (Set String)
processInstances = newIORef zero^.thunk

peerServer :: IO ()
peerServer = do
  sock <- listenOn curlyPort
  proxies <- newIORef ([0..],c'map zero)
  srvState <- newIORef (c'bimap zero)
  timeouts <- newIORef (c'set zero)
  let startTimeout inst = do
        modifyIORef timeouts (touch inst)
        void $ forkIO $ fix $ \again -> do
          x <- readIORef timeouts
          if inst`isKeyIn`x then modifyIORef timeouts (delete inst) >> threadDelay 5000000 >> again
            else do
            logLine Verbose $ format "Freeing stale instance '%s'" inst
            modifyIORef srvState (delete inst)
          
  void $ forkIO $ forever $ do
    (h,addr) <- accept sock
    forkIO $ do
      runConnection_ True h $ fix $ \again -> receive >>= \x -> case x of
        DeclareInstance s t -> do
          port <- liftIO $ runAtomic srvState $ get >>= \m -> case lookup s m of
            Just _ -> return $ Left ("Error: The instance '"+s+"' is already declared")
            _ -> let p = foldr1 (\p ans -> if isKeyIn p (commute m) then ans else p) [curlyPort+1..]
                 in Right p <$ put (insert s p m) 
          sending t port
          case port of
            Right _ -> liftIO $ startTimeout s
            _ -> unit
        RedeclareInstance s p t -> do
          success <- liftIO $ runAtomic srvState $ get >>= \m ->
            if isKeyIn p (commute m) || isKeyIn s m then return False else True <$ put (insert s p m)
          
          liftIO $ if success then startTimeout s >> sending t True
            else runAtomic timeouts (modify $ touch s) >> threadDelay 4000000 >> sending t False
          again
        AskInstance s t -> do
          m <- liftIO $ readIORef srvState
          sending t (maybe (Left $ "Error: Non-existent instance: "+s) Right (lookup s m))
        AskInstances t -> do
          m <- liftIO $ readIORef srvState
          pi <- liftIO $ readIORef processInstances
          let (ours,others) = partition (`elem`pi) (keys m)
          sending t (c'list others + ours)
        DeclareSupply -> do
          lists <- liftIO newChan ; libs <- liftIO newChan
          let askAll = send (Nothing :: Maybe LibraryID) >> readChan lists
              askOne x = case findLib x of
                Just l -> return (Just $ l^.flBytes)
                _ -> send (Just x) >> readChan libs <&> \d -> d<$guard (isLibData x d)
          i <- liftIO $ runAtomic proxies $ id <~ \(i:is,m) -> ((is,insert (i :: Int) (LocalSupply askAll askOne) m),i)
          try unit $ forever $ receive >>= liftIO . \x -> case x of
            SupplyLibraries ls -> writeChan lists ls
            SupplyLibrary l -> writeChan libs l
          liftIO $ modifyIORef proxies $ \(is,m) -> (i:is,delete i m)
        ListLibraries t -> liftIO $ do
          logLine Verbose $ "Request for libraries from "+show addr
          (_,ps) <- readIORef proxies
          sending t . fold =<< sequence [trylog (return []) askAll | LocalSupply askAll _ <- toList ps]
        AskLibrary lid _ -> liftIO $ do
          logLine Verbose $ "Request for library "+show lid+" from "+show addr
          (_,ps) <- readIORef proxies
          let asks = [trylog (return zero) (askOne lid) | LocalSupply _ askOne <- toList ps]
          lib <- foldr (\ask rest -> ask >>= maybe rest (return . Just)) (return Nothing) asks
          ?write (fromMaybe zero lib)
      
peerClient :: IO Handle
peerClient = do
  addrs <- getAddrInfo Nothing (Just "127.0.0.1") (Just (show curlyPort))
  h <- fix $ \run -> trylog (trylog unit peerServer >> run) (connect (head addrs))
  h <$ hSetBuffering h NoBuffering
