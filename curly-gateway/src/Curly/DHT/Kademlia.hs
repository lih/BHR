{-# LANGUAGE DeriveGeneric, TypeFamilies, ScopedTypeVariables, PatternSynonyms, UndecidableInstances #-}
module Curly.DHT.Kademlia (
  DHTIndex,DHTValue,
  DHTInstance,DHTNode,pattern DHTNode,
  JoinResult(..),newDHTInstance,closeDHTInstance,joinDHT,lookupDHT,insertDHT,
  ) where

import Definitive
import Language.Format
import qualified Network.Kademlia as K
import Network.Kademlia (JoinResult(..))

newtype K a = K { getK :: a }
            deriving (Eq,Ord)
instance K.Serialize a => Serializable Bytes (K a) where
  encode p (K a) = encode p (K.toBS a^..chunk)
instance K.Serialize a => Format Bytes (K a) where
  datum = datum >>= (const zero <|> return) . map (K . fst) . K.fromBS . by chunk
instance Format Bytes a => K.Serialize (K a) where
  fromBS b = case (datum^..parser) (b^..chunk) of
    (s,a):_ -> Right (K a,s^.chunk)
    [] -> Left "Parse error"
  toBS (K a) = serialize a^.chunk

class (Ord a,Format Bytes a) => DHTIndex a
class (Eq a,Format Bytes a) => DHTValue a
newtype DHTInstance i a = DHTInstance { _getDHTInstance :: K.KademliaInstance (K i) (K a) }
newtype DHTNode i = DHTNodeImpl { _getDHTNode :: K.Node (K i) }
pattern DHTNode h p i = DHTNodeImpl (K.Node (K.Peer h p) (K i))

newDHTInstance :: (DHTIndex i,DHTValue a,MonadIO m) => Int -> i -> (i -> a -> Bool) -> m (DHTInstance i a)
newDHTInstance p i integ = liftIO $ DHTInstance <$> K.create p (K i) (\(K i) (K a) -> integ i a)
closeDHTInstance :: MonadIO m => DHTInstance i a -> m ()
closeDHTInstance (DHTInstance i) = liftIO $ K.close i
joinDHT :: (DHTIndex i,DHTValue a,MonadIO m) => DHTInstance i a -> DHTNode i -> m JoinResult
joinDHT (DHTInstance i) (DHTNodeImpl n) = liftIO $ K.joinNetwork i n
lookupDHT :: (DHTIndex i,DHTValue a,MonadIO m) => DHTInstance i a -> i -> m (Maybe (a,DHTNode i))
lookupDHT (DHTInstance i) k = liftIO $ K.lookup i (K k) <&> map (getK <#> DHTNodeImpl)
insertDHT :: (DHTIndex i,DHTValue a,MonadIO m) => DHTInstance i a -> i -> a -> m ()
insertDHT (DHTInstance i) k a = liftIO $ K.store i (K k) (K a)
