{-# LANGUAGE DeriveGeneric, TypeFamilies, ScopedTypeVariables, PatternSynonyms, ViewPatterns #-}
module Main(
  main
  ) where

import Definitive
import Language.Format
import System.Environment (getArgs)
import Curly.DHT.Kademlia
import Curly.Core.Security
import Curly.Core.Library
import Curly.Core.VCS
import IO.Network.Socket
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

class Monad m => MonadVC m s | m -> s where
  handleVCRequest :: s -> VCCommand -> m ()

newtype Command m a = Command { runCommand :: m (IO a) }
instance Functor m => Functor (Command m) where
  map f (Command ma) = Command (map2 f ma)
instance Unit m => Unit (Command m) where
  pure a = Command (pure (pure a))
instance SemiApplicative m => SemiApplicative (Command m) where
  Command mf <*> Command mx = Command (liftA2 (<*>) mf mx)
instance Applicative m => Applicative (Command m)
instance MonadIO m => Monad (Command m) where
  join (Command ma) = Command $ join (join (map (liftIO . map runCommand) ma))
instance MonadIO m => MonadIO (Command m) where
  liftIO ma = Command (pure ma)
dhtAction :: Unit m => IO a -> Command m a
dhtAction = Command . pure

newtype DHT_VC m a = DHT_VC { runDHT_VC :: Command m a }
                     deriving (Functor,Unit,SemiApplicative,Applicative)
instance MonadIO m => Monad (DHT_VC m) where join = coerceJoin DHT_VC
instance MonadIO m => MonadVC (DHT_VC m) (Bytes -> IO (), DHTInstance Key Val) where
  handleVCRequest (wr,dht) x = DHT_VC . dhtAction $ let ?write = wr in case x of
    PublishLibrary lid l -> do
      insertMP dht (LibraryKey lid) l
    PublishSource lid s -> do
      insertMP dht (SourceKey lid) s
    GetLibrary l t -> do
      sending t =<< lookupMP dht (LibraryKey l)
    GetSource lid t -> do
      sending t =<< lookupMP dht (SourceKey lid)
    SetBranches pub bs -> do
      insertMP dht (BranchesKey pub) bs
    CreateCommit c t -> do
      let h = hashData (serialize c)
      insertMP dht (CommitKey h) c
      sending t h
    GetCommit h t -> do
      sending t =<< lookupMP dht (CommitKey h)
    ListBranches pub t -> do
      sending t =<< lookupMP dht (BranchesKey pub)

data Key = NodeKey String
         | DataKey ValID
         | LibraryKey LibraryID (WithResponse Bytes)
         | SourceKey LibraryID (WithResponse (Signed String))
         | BranchesKey PublicKey (WithResponse (Signed Branches))
         | CommitKey Hash (WithResponse Commit)
         deriving (Show,Generic)
instance Serializable Key ; instance Format Key
instance Eq Key where a==b = compare a b==EQ
instance Ord Key where compare = comparing serialize
instance DHTIndex Key

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

valID :: Val -> ValID
valID = ValID . hashData . serialize

parMap :: Traversable t => (a -> IO b) -> t a -> IO (t b)
parMap k ta = for ta $ \a -> do
  v <- newEmptyMVar
  forkIO (k a >>= putMVar v)
  return (readMVar v^.thunk)

insertMPBytes :: DHTInstance Key Val -> Bytes -> IO ValID
insertMPBytes dht b
  | bytesSize b<=256 = let v = DataVal b ; cid = valID v in insertDHT dht (DataKey cid) v >> return cid
  | otherwise = let parts = mapAccum_ (\n b -> swap $ splitAt n b) (take 8 $ repeat ((bytesSize b + 7) `div` 8)) b 
                in parMap (insertMPBytes dht) parts >>= \ps -> let v = PartialVal ps ; cid = valID v
                                                               in cid `seq` insertDHT dht (DataKey cid) v >> return cid

lookupMPBytes :: DHTInstance Key Val -> Key -> IO (Maybe Bytes)
lookupMPBytes dht k = do
  x <- lookupDHT dht k
  case x of
    Just (DataVal b,_) -> return (Just b)
    Just (PartialVal ks,_) -> do
      parts <- parMap (lookupMPBytes dht . DataKey) ks
      return (fold <$> sequence parts)
    Nothing -> return Nothing

lookupMP :: (Format a,MonadIO m) => DHTInstance Key Val -> (WithResponse a -> Key) -> m (Maybe a)
lookupMP dht fk = liftIO $ lookupMPBytes dht (fk WithResponse) <&> (>>= matches Just datum)
insertMP :: (Serializable a,MonadIO m) => DHTInstance Key Val -> (WithResponse a -> Key) -> a -> m ()
insertMP dht fk a = liftIO $ insertMPBytes dht (serialize a) >>= \cid -> insertDHT dht (fk WithResponse) (PartialVal [cid])

isValidAssoc (DataKey h) v = valID v == h
isValidAssoc _ _ = True

main = do
  args <- getArgs
  case args of
    (Readable (fromInteger -> serverport):args') -> do
      let isRoot = empty args'
          dhtport = if isRoot then 25464 else fromMaybe (error "No parse for DHT port (expected number)")
                                              (matches Just readable (head args'))
      dht <- newDHTInstance dhtport (NodeKey ("me "+show dhtport)) isValidAssoc
      unless isRoot $ do
        res <- joinDHT dht (DHTNode "127.0.0.1" 25464 (NodeKey ("me "+show 25464)))
        case res of
          JoinSucces -> putStrLn "Successfully joined network node 127.0.0.1:25464"
          _ -> error "Couldnt't reach root node"
      sock <- listenOn serverport
      forever $ do
        (h,_) <- accept sock
        void $ forkIO $ runConnection_ True h $ forever $ do
          x <- receive
          case x of
            PublishLibrary lid l -> do
              insertMP dht (LibraryKey lid) l
            PublishSource lid s -> do
              insertMP dht (SourceKey lid) s
            GetLibrary l t -> do
              sending t =<< lookupMP dht (LibraryKey l)
            GetSource lid t -> do
              sending t =<< lookupMP dht (SourceKey lid)
            SetBranches pub bs -> do
              insertMP dht (BranchesKey pub) bs
            CreateCommit c t -> do
              let h = hashData (serialize c)
              insertMP dht (CommitKey h) c
              sending t h
            GetCommit h t -> do
              sending t =<< lookupMP dht (CommitKey h)
            ListBranches pub t -> do
              sending t =<< lookupMP dht (BranchesKey pub)
    _ -> putStrLn "Usage: curly-dht <server-port> [<dht-port>]"

pattern Readable a <- (matches Just readable -> Just a)
