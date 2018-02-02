{-# LANGUAGE GADTs, DeriveGeneric #-}
module Curly.Core.Security(
  -- * Keys and Secrets
  Access(..),PrivateKey,PublicKey,SharedSecret,KeyFingerprint,Signature,Signed,
  genPrivateKey,publicKey,fingerprint,sharedSecret,signBytes,isValidSignatureFrom,signValue,extractSignedBy,unsafeExtractSigned,
  -- * Encryption/Decryption
  decrypt,encrypt,
  -- * Environment
  KeyStore,curlyKeysFile,getKeyStore,modifyKeyStore,
  -- * Showing and reading formats
  Zesty(..)
  ) where

import Control.DeepSeq (deepseq)
import Curly.Core
import Curly.Core.Documentation
import Data.Bits (xor)
import Data.IORef
import GHC.Conc (threadDelay)
import IO.Filesystem 
import Language.Format
import qualified Codec.Crypto.AES.IO as AES
import qualified Curly.Core.Security.EC as EC
import qualified Crypto.Hash.SHA256 as SHA256
import System.Entropy
import System.IO
import IO.Time

newtype PrivateKey = PrivateKey Integer
            deriving (Eq,Ord)
newtype PublicKey = PublicKey (Integer,Integer)
                  deriving Show
data Signature = Signature Integer Integer
               deriving (Eq,Ord,Generic,Show)
instance Serializable Signature ; instance Format Signature

newtype KeyFingerprint = KeyFingerprint Chunk
                       deriving (Eq,Ord)
data SharedSecret = SharedSecret { readCxt :: AES.AESCtx, writeCxt :: AES.AESCtx }

data Access = Deny | Read | Run | Write | Admin | Almighty
                deriving (Eq,Ord,Enum,Bounded)
instance Show Access where
  show Deny = "none"
  show Read = "read" ; show Run = "execute" ; show Write = "write"
  show Admin = "admin" ; show Almighty = "almighty"
instance Read Access where
  readsPrec _ = readsParser $ foldr1 (<+?)
                [s<$several n | (n,s) <- [("none",Deny),("read",Read),("execute",Run),("write",Write)
                                         ,("admin",Admin),("almighty",Almighty)]]
instance Semigroup Access where (+) = max
instance Monoid Access where zero = minBound
instance Serializable Access where encode a = encode (fromEnum a)
instance Format Access where datum = toEnum <$> datum

-- | This function is useless, but it makes textual representations of data look more
-- "random".
zest :: Bytes -> Bytes
zest bs = pack $ zipWith xor (unpack bs) zestBytes
  where zestBytes = sum $ repeat [
          0xaa,0x48,0xd1,0x13,0x9b,0x4c,0x7e,0xe2,0x22,0x2e,0xac,0x69,0x90,0x4c,0xdb,0x02,
          0x38,0x3c,0x75,0x7a,0x05,0x0b,0x00,0x99,0x59,0x32,0xfa,0x09,0x5d,0x55,0x2b,0xfe,
          0x09,0xc6,0xcc,0x3d,0x49,0xfe,0xb6,0x0e,0xa3,0xd1,0xa2,0xf3,0xcd,0xce,0x0e,0x10,
          0x48,0xa9,0x89,0x83,0x62,0xe0,0x92,0x81,0x17,0xb1,0xae,0x31,0xba,0xd7,0x60,0xfe,
          0x32,0xed,0xb9,0x2d,0xbe,0x4a,0xe2,0x11,0xaa,0x18,0xf5,0x38,0xef,0x19,0x0a,0xac,
          0x95,0xd5,0xd6,0x59,0xf9,0xdb,0x8b,0x63,0xc5,0x8c,0x00,0xc2,0x78,0x12,0x22,0x59,
          0x99,0x35,0xac,0x00,0x7a,0xd0,0xc1,0x1a,0x34,0x29,0x42,0xd3,0x98,0xe2,0x51,0x57,
          0xbb,0xed,0x8f,0xd9,0x24,0xbb,0xd0,0xb1,0x55,0xac,0x04,0x8a,0x29,0x34,0x64,0x8d,
          0x0a,0x07,0x9c,0x87,0xb9,0xf3,0x4f,0x9e,0xa4,0xfd,0xda,0xde,0x2e,0x97,0xf8,0xe7,
          0x55,0x14,0xb9,0xe9,0xc1,0xeb,0xa2,0x48,0x16,0x57,0xe9,0xa3,0x2c,0x27,0x32,0xc7,
          0xd9,0x04,0x25,0xe3,0x7b,0x27,0x31,0x6a,0x49,0x68,0x32,0xe1,0x77,0x0f,0x01,0x22,
          0x06,0xa4,0xc5,0x80,0xa6,0xe8,0x4f,0x0f,0x01,0xc5,0xfc,0x5f,0xc7,0x44,0x0b,0x08,
          0xc5,0x04,0x0e,0x4c,0xf7,0x77,0x14,0x63,0x66,0x41,0xfb,0x35,0x67,0xca,0x9f,0xa8,
          0xdc,0xa2,0x0d,0x28,0x89,0x1b,0x93,0x17,0xe0,0x1b,0x04,0x10,0x0c,0x02,0xbe,0x34,
          0xdd,0xeb,0x87,0xee,0xeb,0x81,0xec,0x1c,0x3f,0xd4,0x91,0xcd,0x22,0xfc,0x9a,0x11,
          0x10,0xc4,0x16,0x86,0x23,0x7e,0xcd,0x3d,0x63,0x16,0x78,0x91,0xfb,0xf2,0xe2,0xd4
          ]

newtype Zesty a = Zesty a
instance Serializable a => Show (Zesty a) where
  show (Zesty a) = show (B64Chunk (zest (serialize a)^.chunk))
instance Format a => Read (Zesty a) where
  readsPrec _ = readsParser ((readable <&> \(B64Chunk c) -> zest (c^..chunk)) >*> (Zesty<$>datum))

fpSize = 8
instance Show KeyFingerprint where show (KeyFingerprint f) = show (B64Chunk f)
instance FormatArg KeyFingerprint where argClass _ = 'k'
instance Bounded KeyFingerprint where
  minBound = KeyFingerprint (pack [0 :: Word8 | _ <- [1..fpSize]])
  maxBound = KeyFingerprint (pack [0xff :: Word8 | _ <- [1..fpSize]])

instance Serializable PrivateKey where encode = coerceEncode PrivateKey
instance Format PrivateKey where datum = coerceDatum PrivateKey
instance Serializable PublicKey where encode = coerceEncode PublicKey
instance Format PublicKey where datum = coerceDatum PublicKey
instance Serializable KeyFingerprint where encode (KeyFingerprint f) = f^.chunkBuilder
instance Format KeyFingerprint where datum = KeyFingerprint<$>getChunk fpSize

chunkToInteger c = fromMaybe 0 $ matches Just datum
                   $ serialize (chunkSize c) + c^..chunk

genPrivateKey :: MonadIO m => m PrivateKey
genPrivateKey = liftIO $ PrivateKey . chunkToInteger <$> getEntropy 32

curveOrder = EC.getr EC.baseCurve

signBytes :: MonadIO m => PrivateKey -> Bytes -> m Signature
signBytes (PrivateKey priv) bs = liftIO $ do
  let h = chunkToInteger (SHA256.hashlazy bs)
      genSig = do
        k <- (`mod`curveOrder) . chunkToInteger <$> getEntropy 32
        let (i,_) = (EC.pmul EC.basePoint k)
            x = i`mod`curveOrder
        if x==0 then genSig else do
          let k' = inverseIn curveOrder k
              y = (k' * (h + priv * x))`mod`curveOrder
          if y==0 then genSig else return (Signature x y)
  genSig
isValidSignatureFrom :: PublicKey -> Signature -> Bytes -> Bool
isValidSignatureFrom (PublicKey pub) (Signature x y) bs =
  let h = chunkToInteger (SHA256.hashlazy bs)
      y' = inverseIn curveOrder y
      (i,_) = EC.padd
              (EC.basePoint `EC.pmul` ((h * y') `mod` curveOrder))
              (pub `EC.pmul` ((x * y') `mod` curveOrder))
  in (i`mod`curveOrder) == x
inverseIn :: (Integral t,Num t,Disjonctive t,Semiring t) => t -> t -> t
inverseIn p n = let (u,_,_) = bezout n p in u
bezout :: (Integral t,Num t,Disjonctive t,Semiring t) => t -> t -> (t,t,t)
bezout a 0 = (1,0,a)
bezout a b = (v',u'-(k*v'),g)
  where (u',v',g) = bezout b q
        (k,q) = a`divMod`b

data Signed a = Signed a Signature
              deriving (Eq,Ord,Show,Generic)
instance Serializable a => Serializable (Signed a)
instance Format a => Format (Signed a)
unsafeExtractSigned :: Signed a -> a
unsafeExtractSigned (Signed a _) = a
extractSignedBy :: Serializable a => PublicKey -> Signed a -> Maybe a
extractSignedBy pub (Signed a s) | isValidSignatureFrom pub s (serialize a) = Just a
                                 | otherwise = Nothing
signValue :: (MonadIO m,Serializable a) => PrivateKey -> a -> m (Signed a)
signValue priv a = Signed a <$> signBytes priv (serialize a)

timingRef :: IORef Seconds
timingRef = thunk $^ newIORef 0

publicKey :: PrivateKey -> PublicKey
publicKey (PrivateKey n) = thunk $^ do
  let ret = EC.pmul EC.basePoint n
  start <- currentTime
  serialize ret `deepseq` unit
  end <- currentTime
  let time = end - start
  -- This function pads the key computing time to the maximum observed
  -- on this instance. This protects the cryptosystem from timing attacks
  -- where a hacker can guess a private key's range by measuring successive
  -- response times from a server, by ensuring that those response times converge
  -- towards a maximum value.
  logLine Debug $ format "Public key computing time: %fs" time
  io <- runAtomic timingRef $ get >>= \m -> if time > m then unit <$ put time else return (waitTill (start+m))
  io
  return (PublicKey ret)

fingerprint :: PublicKey -> KeyFingerprint
fingerprint k = KeyFingerprint (SHA256.hashlazy (serialize k) & pack . shorten . shorten . unpack)
  where shorten (x:y:t) = x`xor`y:shorten t
        shorten t = t

clientCtrStart :: Chunk
clientCtrStart = ([0x87,0xa8,0xee,0x6d,0x7c,0xf0,0x40,0xd2
                  ,0xc8,0x08,0x23,0x2f,0xfe,0xdb,0x48,0x43]++zero)^.chunk
serverCtrStart :: Chunk
serverCtrStart = ([0x96,0xcb,0x56,0xb4,0x7d,0xb2,0xb7,0xaa
                  ,0x61,0xc2,0xc6,0x5e,0x69,0xaa,0x66,0x65]++zero)^.chunk
sharedSecret :: MonadIO m => Bool -> PrivateKey -> PublicKey -> m SharedSecret
sharedSecret isClient (PrivateKey priv) (PublicKey pub) = liftIO $ do
  let kh = SHA256.hashlazy (serialize (EC.pmul pub priv))
      mkCtx = AES.newCtx AES.CTR kh . \isClt -> if isClt then clientCtrStart else serverCtrStart
  logLine Debug $ format "Shared secret : %s" (show (B64Chunk kh))
  SharedSecret <$> mkCtx isClient AES.Decrypt <*> mkCtx (not isClient) AES.Encrypt

decrypt :: (MonadIO m,Format a, ?secret :: SharedSecret) => ParserT Bytes m a
decrypt = receive >*> do
  remaining >>= liftIO . AES.crypt (readCxt ?secret) . by chunk >>= runStreamState . put . yb chunk
  receive
encrypt :: (MonadIO m,Serializable a,?secret :: SharedSecret) => a -> m Bytes
encrypt a = liftIO $ yb chunk <$> AES.crypt (writeCxt ?secret) (serialize a ^. chunk)

type KeyStore = Map String (KeyFingerprint,PublicKey,Maybe PrivateKey,Metadata,Map String Access)

identities :: IORef KeyStore
identities = thunk $^ do
  modifyPermissions curlyKeysFile (set groupPerms zero . set otherPerms zero)
  ids <- trylog (return zero) (readFormat curlyKeysFile)
  newIORef ids <* watchFile curlyKeysFile reloadKeyStore
  
reloadKeyStore :: IO ()
reloadKeyStore = do
  logLine Debug "Reloading key store"
  ks <- trylog (return zero) $ readFormat curlyKeysFile
  runKeyState $ put ks

runKeyState :: MonadIO m => State KeyStore a -> m a
runKeyState = liftIO . runAtomic identities
getKeyStore :: MonadIO m => m KeyStore
getKeyStore = runKeyState get
modifyKeyStore :: MonadIO m => (KeyStore -> KeyStore) -> m ()
modifyKeyStore m = liftIO $ while $ trylog (threadDelay 1000 >> return True) $ False<$ do
  withFile curlyKeysFile ReadWriteMode $ \h -> do
    -- This little trick keeps GHC from prematurely closing the handle
    -- when the parser reaches the end of the byte stream
    sz <- between (hSeek h SeekFromEnd 0) (hSeek h AbsoluteSeek 0) (hTell h)
    oldFile <- take (fromIntegral sz) <$> readHBytes h
    let ks = fromMaybe zero (matches Just datum oldFile)
        ks' = m ks
        newFile = serialize ks'
    runKeyState (put ks')
    newFile `deepseq` return ()
    hSeek h AbsoluteSeek 0
    hSetFileSize h 0
    writeHBytes h newFile
