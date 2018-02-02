module Curly.Core.Peers where

import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Curly.Core
import Curly.Core.Documentation
import Curly.Core.Library
import Data.IORef 
import GHC.Conc (threadDelay)
import IO.Network.Socket
import Language.Format
import System.IO (hSetBuffering,BufferMode(..))

data PeerPacket = DeclareInstance String (WithResponse (Either String PortNumber))
                | RedeclareInstance String PortNumber (WithResponse Bool)
                | AskInstance String (WithResponse (Either String PortNumber))
                | AskInstances (WithResponse [String])
                | AskLibrary LibraryID (WithResponse Bytes)
                deriving Generic
instance Serializable PortNumber where
  encode = encode . c'int . fromIntegral
instance Format PortNumber where
  datum = fromIntegral . c'int <$> datum
instance Serializable PeerPacket
instance Format PeerPacket

processInstances :: IORef (Set String)
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
      
peerClient :: IO Handle
peerClient = do
  addrs <- getAddrInfo Nothing (Just "127.0.0.1") (Just (show curlyPort))
  h <- fix $ \run -> trylog (trylog unit peerServer >> run) (connect (head addrs))
  h <$ hSetBuffering h NoBuffering
