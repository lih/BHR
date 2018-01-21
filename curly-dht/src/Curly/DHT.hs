{-# LANGUAGE DeriveGeneric, TypeFamilies, ScopedTypeVariables, PatternSynonyms, ViewPatterns, ExistentialQuantification, TypeOperators #-}
module Main(
  main
  ) where

import Definitive
import Language.Format
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
      let dhtInstance dhtport = newDHTInstance (fromIntegral dhtport) (OtherKey (NodeKey ("curly-vc "+show dhtport))) isValidAssoc
      backend <- case a of
        DHTRoot port -> do
          dht <- dhtInstance port
          return (VCSB_Native "dht://" dht (\(DHT_VC m) -> m))
        DHTBackend port srv srv_port -> do
          dht <- dhtInstance port
          res <- joinDHT dht (DHTNode srv srv_port (OtherKey (NodeKey ("curly-vc "+show srv_port))))
          case res of
            JoinSucces -> putStrLn $ "Successfully joined network node "+srv+":"+show srv_port
            _ -> error "Couldnt't reach root node"
          return (VCSB_Native "dht://" dht (\(DHT_VC m) -> m))
        FileBackend base -> do
          return (VCSB_Native base base (\(File_VC m) -> m))

      sock <- listenOn 5402
      forever $ do
        (h,_) <- accept sock
        void $ forkIO $ runConnection_ True h $ forever $ vcServer backend

