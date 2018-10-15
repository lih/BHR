module Curly.Core.Peers where

import Control.Concurrent (forkIO)
import Curly.Core
import Data.IORef 
import GHC.Conc (threadDelay)
import IO.Network.Socket
import Language.Format
import System.IO (hSetBuffering,BufferMode(..))

type InstanceName = String
type PeerErrorMessage = String
data PeerPacket = DeclareInstance InstanceName (Proxy (Either PeerErrorMessage PeerPort))
                | RedeclareInstance InstanceName PeerPort (Proxy Bool)
                | AskInstance InstanceName (Proxy (Either PeerErrorMessage PeerPort))
                | AskInstances (Proxy [InstanceName])
                deriving Generic

newtype PeerPort = PeerPort { getPeerPortNumber :: PortNumber }
instance Serializable Word8 Builder Bytes PeerPort where
  encode p = encode p . c'int . fromIntegral . getPeerPortNumber
instance Format Word8 Builder Bytes PeerPort where
  datum = PeerPort . fromIntegral . c'int <$> datum
instance Serializable Word8 Builder Bytes PeerPacket
instance Format Word8 Builder Bytes PeerPacket

processInstances :: IORef (Set InstanceName)
processInstances = newIORef zero^.thunk

peerServer :: IO ()
peerServer = do
  sock <- listenOn curlyPort
  srvState <- newIORef (c'bimap zero)
  timeouts <- newIORef (c'set zero)
  let startTimeout inst = do
        modifyIORef timeouts (touch inst)
        void $ forkIO $ fix $ \again -> do
          x <- readIORef timeouts
          if inst`isKeyIn`x then modifyIORef timeouts (delete inst) >> threadDelay 5000000 >> again
            else do
            logLine Chatty $ format "Freeing stale instance '%s'" inst
            modifyIORef srvState (delete inst)
          
  void $ forkIO $ forever $ do
    (h,_) <- accept sock
    forkIO $ do
      runConnection_ True h $ fix $ \again -> receive >>= \case
        DeclareInstance name t -> do
          port <- liftIO $ runAtomic srvState $ get >>= \m -> case lookup name m of
            Just _ -> return $ Left ("Error: The instance '"+name+"' is already declared")
            _ -> let firstAvailablePort = foldr1 (\p ans -> if isKeyIn p (commute m) then ans else p) [curlyPort+1..]
                 in Right firstAvailablePort <$ put (insert name firstAvailablePort m) 
          sending t (PeerPort <$> port)
          case port of
            Right _ -> liftIO $ startTimeout name
            _ -> unit
        RedeclareInstance name (PeerPort p) t -> do
          success <- liftIO $ runAtomic srvState $ get >>= \m ->
            if isKeyIn p (commute m) || isKeyIn name m then return False else True <$ put (insert name p m)
          
          liftIO $ if success then startTimeout name
            else runAtomic timeouts (modify $ touch name) >> threadDelay 4000000
          sending t success
          again
        AskInstance name t -> do
          m <- liftIO $ readIORef srvState
          sending t (maybe (Left $ "Error: Non-existent instance: "+name) (Right . PeerPort) (lookup name m))
        AskInstances t -> do
          m <- liftIO $ readIORef srvState
          procI <- liftIO $ readIORef processInstances
          let (ours,others) = partition (`elem`procI) (keys m)
          sending t (c'list others + ours)
      
peerClient :: IO Handle
peerClient = do
  addrs <- getAddrInfo Nothing (Just "127.0.0.1") (Just (show curlyPort))
  h <- fix $ \run -> trylog (trylog unit peerServer >> run) (connect (head addrs))
  h <$ hSetBuffering h NoBuffering
