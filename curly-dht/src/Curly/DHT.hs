{-# LANGUAGE DeriveGeneric, TypeFamilies, ScopedTypeVariables, PatternSynonyms, ViewPatterns, ExistentialQuantification, TypeOperators #-}
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
import Curly.Core (pretty)
import IO.Network.Socket
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import System.Console.GetOpt (OptDescr(..),ArgDescr(..),usageInfo,getOpt,ArgOrder(..))
import qualified Prelude as P

instance Functor OptDescr where map = P.fmap

class Monad m => MonadVC m s | m -> s where
  handleVCRequest :: (Bytes -> IO ()) -> s -> VCCommand -> m ()

newtype Command m a = Command { runCommand :: (IO :.: m) a }
                      deriving (Functor,Unit,SemiApplicative,Applicative)
instance (Monad m,Traversable m) => Monad (Command m) where join = coerceJoin Command
dhtAction :: Unit m => IO a -> Command m a
dhtAction = Command . Compose . map pure

newtype DHT_VC m a = DHT_VC { runDHT_VC :: Command m a }
                     deriving (Functor,Unit,SemiApplicative,Applicative)
instance (Monad m,Traversable m) => Monad (DHT_VC m) where join = coerceJoin DHT_VC
instance (Monad m,Traversable m) => MonadVC (DHT_VC m) (DHTInstance Key Val) where
  handleVCRequest wr dht x = DHT_VC . dhtAction $ let ?write = wr in case x of
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

newtype File_VC m a = File_VC { runFile_VC :: Command m a }
                    deriving (Functor,Unit,SemiApplicative,Applicative)
instance (Monad m,Traversable m) => Monad (File_VC m) where join = coerceJoin File_VC
storeFile :: Serializable a => String -> (WithResponse a -> Key) -> a -> IO ()
storeFile base k v = do
  let keyName = pretty (serialize (k WithResponse)^.chunk)
  writeSerial (base+"/"+keyName) v
loadFile :: Format a => String -> (WithResponse a -> Key) -> IO (Maybe a)
loadFile base k = do
  let keyName = pretty (serialize (k WithResponse)^.chunk)
  try (return Nothing) (Just <$> readFormat (base+"/"+keyName))
instance (Monad m,Traversable m) => MonadVC (File_VC m) String where
  handleVCRequest wr path x = File_VC . dhtAction $ let ?write = wr in case x of
    PublishLibrary lid l  -> storeFile path (LibraryKey lid) l
    PublishSource lid s   -> storeFile path (SourceKey lid)  s
    GetLibrary l t        -> sending t =<< loadFile path (LibraryKey l)
    GetSource lid t       -> sending t =<< loadFile path (SourceKey lid)
    SetBranches pub bs    -> storeFile path (BranchesKey pub) bs
    CreateCommit c t      -> do
      let h = hashData (serialize c)
      storeFile path (CommitKey h) c
      sending t h
    GetCommit h t         -> sending t =<< loadFile path (CommitKey h)
    ListBranches pub t    -> sending t =<< loadFile path (BranchesKey pub)

data VCBackend = forall m s. MonadVC m s => VCBackend s (m () -> IO ())
vcRequest wr (VCBackend s cast) cmd = cast (handleVCRequest wr s cmd)

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

data DHTAction = DHTBackend PortNumber String PortNumber
               | DHTRoot PortNumber
               | FileBackend String
data DHTOpt = Action DHTAction | Help

dhtOpts = [
  Option "h" ["help"] (NoArg Help) "Shows the help menu",
  Option "" ["dht-root"] (OptArg (Action . maybe (DHTRoot 25464) (DHTRoot . read)) "PORT") "Start a root DHT server",
  Option "" ["dht-client"] (ReqArg (Action . parseClient) "PORT:SERVER:SERVER-PORT") "Start a DHT client",
  Option "" ["filesystem"] (ReqArg (Action . FileBackend) "DIRECTORY") "Start a local filesystem VC"
  ]
  where parseClient c = fromMaybe (error $ "Could not parse DHT client: "+c)
                        $ matches Just (liftA3 DHTBackend
                                        number
                                        (single ':' >> many1' (satisfy (/=':')))
                                        (single ':' >> number)) c

main = do
  (args,_,_) <- getOpt (ReturnInOrder (const [])) (map2 pure dhtOpts) <$> getArgs
  case concat args of
    Help:_ -> putStrLn (usageInfo "curly-vc" dhtOpts)
    Action a:_ -> do
      let dhtInstance dhtport = newDHTInstance (fromIntegral dhtport) (NodeKey ("curly-vc "+show dhtport)) isValidAssoc
      backend <- case a of
        DHTRoot port -> do
          dht <- dhtInstance port
          return (VCBackend dht (\(DHT_VC (Command (Compose m))) -> getId<$>m))
        DHTBackend port srv srv_port -> do
          dht <- dhtInstance port
          res <- joinDHT dht (DHTNode srv srv_port (NodeKey ("curly-vc "+show srv_port)))
          case res of
            JoinSucces -> putStrLn $ "Successfully joined network node "+srv+":"+show srv_port
            _ -> error "Couldnt't reach root node"
          return (VCBackend dht (\(DHT_VC (Command (Compose m))) -> getId<$>m))
        FileBackend base -> do
          return (VCBackend base (\(File_VC (Command (Compose m))) -> getId<$>m))

      sock <- listenOn 5402
      forever $ do
        (h,_) <- accept sock
        void $ forkIO $ runConnection_ True h $ forever $ do
          x <- receive
          liftIO (vcRequest ?write backend x)

