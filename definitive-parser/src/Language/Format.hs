{-# LANGUAGE ScopedTypeVariables, TypeFamilies, ExistentialQuantification, ImplicitParams, DefaultSignatures, UndecidableInstances #-}
module Language.Format (
  -- * You'll need this
  module Language.Parser,
  Gen.Generic,
  
  -- * Serialization
  SerialStream(..),Serializable(..),Format(..),Builder,bytesBuilder,chunkBuilder,serialize,serial,stringBytes,
  -- ** Convenience functions
  defaultEncode,defaultDatum,
  word8,Word8,Word16,Word32,Word64,LittleEndian(..),encodeAlt,FormatAlt(..),datumOf,getChunk,
  writeSerial,readFormat,writeHSerial,readHFormat,
  -- ** Bidirectional serialization
  runConnection,runConnection_,send,receive,
  Proxy(..),exchange,sending,

  -- * GND replacement for GHC 7.8 and up
  coerceEncode,coerceDatum
  ) where

import Data.Bits (shiftR,shiftL,xor)
import Data.ByteString.Lazy.Builder

import Data.Word
import Definitive
import Language.Parser hiding (uncons)
import qualified Control.DeepSeq as DeepSeq
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as UTF8 
import qualified Data.Monoid as M
import qualified Prelude as P
import System.IO (hFlush,withFile,IOMode(..))
import GHC.IO.Handle (hClose)
import Unsafe.Coerce
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek,poke)
import Foreign.Marshal.Alloc (alloca)
import qualified GHC.Generics as Gen

instance ParseToken Word8 where tokenPayload c = c

class (ParseStream char stream, TokenPayload char ~ Word8, Monoid builder) => SerialStream char builder stream | stream -> builder char where
  encodeByte :: Proxy stream -> Word8 -> builder
  toSerialStream :: builder -> stream
instance ParseStream Word8 Bytes
instance SerialStream Word8 Builder Bytes where
  encodeByte _ = word8
  toSerialStream = toLazyByteString

class SerialStream char builder stream => Serializable char builder stream t | t stream -> builder char where
  encode :: Proxy stream -> t -> builder
  default encode :: (Gen.Generic t, GenSerializable char builder stream (Gen.Rep t)) => Proxy stream -> t -> builder
  encode = defaultEncode
class Serializable char builder stream t => Format char builder stream t | t stream -> builder char where
  datum :: Parser stream t
  default datum :: (Gen.Generic t, GenFormat char builder stream (Gen.Rep t)) => Parser stream t
  datum = defaultDatum

defaultEncode :: (Gen.Generic t, GenSerializable char builder stream (Gen.Rep t)) => Proxy stream -> t -> builder
defaultEncode p a = genEncode p (Gen.from a)
defaultDatum :: (Gen.Generic t, GenFormat char builder stream (Gen.Rep t)) => Parser stream t
defaultDatum = Gen.to <$> genDatum

class SerialStream char builder stream => GenSerializable char builder stream f | stream -> builder char where
  genEncode :: Proxy stream -> f a -> builder
class GenSerializable char builder stream f => GenFormat char builder stream f | stream -> builder char where
  genDatum :: Parser stream (f a)

class GenAlt f where
  altTotal :: f a -> Int
  altTotal _ = 1
class (GenAlt f,GenSerializable char builder stream f) => GenSerAlt char builder stream f | stream -> builder char where
  altEncode :: Proxy stream -> f a -> Int -> builder
  altEncode p fa n = encode p n + genEncode p fa
class (GenSerAlt char builder stream f,GenFormat char builder stream f) => GenFormatAlt char builder stream f | stream -> builder char where
  altDatum :: Int -> Parser stream (f a)
  altDatum 0 = genDatum
  altDatum _ = zero

instance GenAlt Gen.V1
instance SerialStream char builder stream => GenSerializable char builder stream Gen.V1 where genEncode = undefined
instance SerialStream char builder stream => GenFormat char builder stream Gen.V1 where genDatum = undefined
instance SerialStream char builder stream => GenSerAlt char builder stream Gen.V1
instance SerialStream char builder stream => GenFormatAlt char builder stream Gen.V1

instance GenAlt Gen.U1
instance SerialStream char builder stream => GenSerializable char builder stream Gen.U1 where genEncode _ = zero
instance SerialStream char builder stream => GenFormat char builder stream Gen.U1 where genDatum = pure Gen.U1
instance SerialStream char builder stream => GenSerAlt char builder stream Gen.U1
instance SerialStream char builder stream => GenFormatAlt char builder stream Gen.U1

instance GenAlt (Gen.K1 i c)
instance (SerialStream char builder stream, Serializable char builder stream t) => GenSerializable char builder stream (Gen.K1 i t) where
  genEncode p (Gen.K1 t) = encode p t
instance (SerialStream char builder stream, Format char builder stream t) => GenFormat char builder stream (Gen.K1 i t) where
  genDatum = Gen.K1 <$> datum
instance (SerialStream char builder stream, Serializable char builder stream t) => GenSerAlt char builder stream (Gen.K1 i t)
instance (SerialStream char builder stream, Format char builder stream t) => GenFormatAlt char builder stream (Gen.K1 i t)

instance GenAlt f => GenAlt (Gen.M1 i t f) where altTotal (Gen.M1 fa) = altTotal fa
instance (SerialStream char builder stream, GenSerializable char builder stream f) => GenSerializable char builder stream (Gen.M1 i t f) where
  genEncode p (Gen.M1 x) = genEncode p x
instance (SerialStream char builder stream, GenFormat char builder stream f) => GenFormat char builder stream (Gen.M1 i t f) where
  genDatum = Gen.M1 <$> genDatum
instance (SerialStream char builder stream, GenSerAlt char builder stream f) => GenSerAlt char builder stream (Gen.M1 i t f) where
  altEncode p (Gen.M1 fa) = altEncode p fa
instance (SerialStream char builder stream, GenFormatAlt char builder stream f) => GenFormatAlt char builder stream (Gen.M1 i t f) where
  altDatum n = Gen.M1 <$> altDatum n

instance GenAlt (f Gen.:*: g)
instance (GenSerializable char builder stream f, GenSerializable char builder stream g) => GenSerializable char builder stream (f Gen.:*: g) where
  genEncode p (fa Gen.:*: ga) = genEncode p fa + genEncode p ga
instance (GenFormat char builder stream f, GenFormat char builder stream g) => GenFormat char builder stream (f Gen.:*: g) where
  genDatum = liftA2 (Gen.:*:) genDatum genDatum
instance (GenSerializable char builder stream f,GenSerializable char builder stream g) => GenSerAlt char builder stream (f Gen.:*: g)
instance (GenFormat char builder stream f, GenFormat char builder stream g) => GenFormatAlt char builder stream (f Gen.:*: g) where

instance (GenAlt f,GenAlt g) => GenAlt (f Gen.:+: g) where
  altTotal = \_ -> tot
    where tot = altTotal (undefined :: f a) + altTotal (undefined :: g a)
instance (GenSerAlt char builder stream f,GenSerAlt char builder stream g) => GenSerializable char builder stream (f Gen.:+: g) where
  genEncode p (Gen.L1 fa) = altEncode p fa 0
  genEncode p (Gen.R1 ga) = altEncode p ga (altTotal (undefined :: f a))
instance (GenFormatAlt char builder stream f,GenFormatAlt char builder stream g) => GenFormat char builder stream (f Gen.:+: g) where
  genDatum = datum >>= altDatum
instance (GenSerAlt char builder stream f,GenSerAlt char builder stream g) => GenSerAlt char builder stream (f Gen.:+: g) where
  altEncode p (Gen.L1 f) n = altEncode p f n
  altEncode p (Gen.R1 g) n = altEncode p g (altTotal (undefined :: f a)+n) 
instance (GenFormatAlt char builder stream f,GenFormatAlt char builder stream g) => GenFormatAlt char builder stream (f Gen.:+: g) where
  altDatum = \n -> if n < tot then Gen.L1 <$> altDatum n else Gen.R1 <$> altDatum (n - tot)
    where tot = altTotal (undefined :: f a)
writeHSerial :: Serializable Word8 Builder Bytes a => Handle -> a -> IO ()
writeHSerial h a = ser`DeepSeq.deepseq`writeHBytes h ser
  where ser = serialize a
writeSerial :: Serializable Word8 Builder Bytes a => String -> a -> IO ()
writeSerial h a = ser`DeepSeq.deepseq`writeBytes h ser
  where ser = serialize a
readHFormat :: Format Word8 Builder Bytes a => Handle -> IO a
readHFormat h = maybe (error "Coudn't parse format") return . matches Just datum . DeepSeq.force =<< readHBytes h
readFormat :: Format Word8 Builder Bytes a => String -> IO a
readFormat f = withFile f ReadMode $ readHFormat

coerceEncode :: forall char builder stream t t'. Serializable char builder stream t => (t -> t') -> (Proxy stream -> t' -> builder)
coerceEncode _ = unsafeCoerce (encode :: Proxy stream -> t -> builder)
coerceDatum :: forall char builder stream t t'. Format char builder stream t => (t -> t') -> (Parser stream t')
coerceDatum _ = unsafeCoerce (datum :: Parser stream t)

serialize :: forall char builder stream t. Serializable char builder stream t => t -> stream
serialize = toSerialStream . encode (Proxy :: Proxy stream)

serial :: (Format char builder stream t,Serializable char builder stream t') => Traversal t t' stream stream
serial = prism (datum^.from parser & \f a -> map snd (foldr (const . Right) (Left a) (f a))) (const serialize)

bytesBuilder :: Bytes:<->:Builder
bytesBuilder = iso lazyByteString toLazyByteString
chunkBuilder :: Chunk:<->:Builder
chunkBuilder = iso byteString (by chunk.toLazyByteString)

stringBytes :: String -> Bytes
stringBytes s = foldMap (encode (Proxy :: Proxy Bytes)) s^..bytesBuilder

encodeAlt :: Serializable char builder stream a => Proxy stream -> Word8 -> a -> builder
encodeAlt p n a = encodeByte p n + encode p a
data FormatAlt char builder stream a = forall b. Format char builder stream b => FormatAlt (b -> a)
datumOf :: SerialStream char builder stream => [FormatAlt char builder stream a] -> Parser stream a
datumOf l = datum >>= \x -> case drop (fromIntegral (x :: Word8)) l of
  (FormatAlt h:_) -> h<$>datum
  _ -> zero
getChunk :: Int -> Parser Bytes Chunk
getChunk sz = splitAt sz<$>remaining >>= \(h,t) -> guard (bytesSize h == sz) >> (by chunk h <$ runStreamState (put t))

runConnection :: (MonadIO io,Monoid m) =>
                 (a -> m) -- ^ An accumulating function
                 -> Bool  -- ^ Close handle after parsing ?
                 -> Handle -- ^ A bidirectional Handle
                 -> ((?write :: Bytes -> IO ()) => ParserT Bytes io a) -- ^ The parser to run (has access to 'receive' and 'send' actions)
                 -> io m
runConnection x close h srv = do
  bs <- liftIO (readHBytes h)
  let ?write = \bs' -> writeHChunk h (bs'^.chunk) >> hFlush h
  matchesT x srv bs <* liftIO (when close (hClose h))
runConnection_ :: MonadIO io => Bool -> Handle -> ((?write :: Bytes -> IO ()) => ParserT Bytes io a) -> io ()
runConnection_ = runConnection (const ())

send :: (Serializable Word8 Builder Bytes t, MonadIO m, ?write :: Bytes -> IO ()) =>  t -> m ()
send x = liftIO (?write (serialize x))
receive :: (Format char builder stream t,Monad m) => ParserT stream m t
receive = generalize datum

-- | A proxy type to specify response types
data Proxy a = Proxy
             deriving (Show,Eq,Ord)
instance SerialStream char builder stream => Serializable char builder stream (Proxy a) where encode Proxy = zero
instance SerialStream char builder stream => Format char builder stream (Proxy a) where datum = return Proxy
exchange :: (MonadIO m,?write :: Bytes -> IO (),Format char builder stream a,Serializable Word8 Builder Bytes b) => (Proxy a -> b) -> ParserT stream m a
exchange f = do
  send (f Proxy)
  receive
sending :: (MonadIO m,?write :: Bytes -> IO (),Serializable Word8 Builder Bytes a) => Proxy a -> a -> m ()
sending _ = send

wordToFloat :: Word32 -> Float
wordToFloat w = by thunk $ alloca $ \p -> poke p w >> peek (castPtr p)
floatToWord :: Float -> Word32
floatToWord w = by thunk $ alloca $ \p -> poke p w >> peek (castPtr p)
wordToDouble :: Word64 -> Double
wordToDouble w = by thunk $ alloca $ \p -> poke p w >> peek (castPtr p)
doubleToWord :: Double -> Word64
doubleToWord w = by thunk $ alloca $ \p -> poke p w >> peek (castPtr p)

newtype LittleEndian t = LittleEndian { fromLittleEndian :: t }

instance Semigroup Word8 ; instance Monoid Word8
instance Semigroup Word16 ; instance Monoid Word16
instance Semigroup Word32 ; instance Monoid Word32
instance Semigroup Word64 ; instance Monoid Word64
instance Semiring Word8 where (*) = (P.*)
instance Ring Word8 where one = 1
instance Semiring Word16 where (*) = (P.*)
instance Ring Word16 where one = 1
instance Semiring Word32 where (*) = (P.*)
instance Ring Word32 where one = 1
instance Semiring Word64 where (*) = (P.*)
instance Ring Word64 where one = 1
instance Disjonctive Word8 where
  negate = P.negate ; (-) = (P.-)
instance Disjonctive Word16 where
  negate = P.negate ; (-) = (P.-)
instance Disjonctive Word32 where
  negate = P.negate ; (-) = (P.-)
instance Disjonctive Word64 where
  negate = P.negate ; (-) = (P.-)

instance Semigroup Builder where (+) = M.mappend
instance Monoid Builder where zero = M.mempty

instance Serializable Word8 Builder Bytes Char where
  encode _ = charUtf8
instance Format Word8 Builder Bytes Char where
  datum = runStreamState (gets UTF8.uncons) >>= \x -> case x of
    Just (c,t) -> c <$ runStreamState (put t)
    Nothing -> zero
instance SerialStream char builder stream => Serializable char builder stream Word8 where
  encode = encodeByte
instance SerialStream char builder stream => Format char builder stream Word8 where
  datum = token
instance Serializable Word8 Builder Bytes Bytes where
  encode p b = encode p (fromIntegral (BS.length b) :: Int) + b^.bytesBuilder
instance Format Word8 Builder Bytes Bytes where
  datum = do
    sz <- datum
    x <- remaining
    let (h,t) = splitAt sz x
    h <$ runStreamState (put t)
instance Serializable Word8 Builder Bytes Word16 where
  encode _ = word16BE
instance Format Word8 Builder Bytes Word16 where
  datum = mkW16 <$> doTimes 2 datum
    where mkW16 l = sum $ zipWith shiftL (map fi l) [8,0]
          fi = fromIntegral :: Word8 -> Word16
instance Serializable Word8 Builder Bytes (LittleEndian Word16) where
  encode _ = word16LE . fromLittleEndian
instance Format Word8 Builder Bytes (LittleEndian Word16) where
  datum = LittleEndian . mkW16 <$> doTimes 2 datum
    where mkW16 l = sum $ zipWith shiftL (map fi l) [0,8]
          fi = fromIntegral :: Word8 -> Word16
instance Serializable Word8 Builder Bytes Word32 where
  encode _ = word32BE
instance Format Word8 Builder Bytes Word32 where
  datum = mkW32 <$> doTimes 4 datum
    where mkW32 l = sum $ zipWith shiftL (map fi l) [24,16,8,0]
          fi = fromIntegral :: Word8 -> Word32
instance Serializable Word8 Builder Bytes (LittleEndian Word32) where
  encode _ = word32LE . fromLittleEndian
instance Format Word8 Builder Bytes (LittleEndian Word32) where
  datum = LittleEndian . mkW32 <$> doTimes 4 datum
    where mkW32 l = sum $ zipWith shiftL (map fi l) [0,8,16,24]
          fi = fromIntegral :: Word8 -> Word32
instance Serializable Word8 Builder Bytes Word64 where
  encode _ = word64BE
instance Format Word8 Builder Bytes Word64 where
  datum = mkW64 <$> doTimes 8 datum
    where mkW64 l = sum $ zipWith shiftL (map fi l) [56,48,40,32,24,16,8,0]
          fi = fromIntegral :: Word8 -> Word64
instance Serializable Word8 Builder Bytes (LittleEndian Word64) where
  encode _ = word64LE . fromLittleEndian
instance Format Word8 Builder Bytes (LittleEndian Word64) where
  datum = LittleEndian . mkW64 <$> doTimes 8 datum
    where mkW64 l = sum $ zipWith shiftL (map fi l) [0,8,16,24,32,40,48,56]
          fi = fromIntegral :: Word8 -> Word64
instance SerialStream char builder stream => Serializable char builder stream Int where
  encode p n = foldMap (encodeByte p) bytes'
    where bytes = map fromIntegral $ takeWhile (>0) $ iterate (`shiftR`8) n
          bytes' = case bytes of
            []             -> [0]
            [h] | h < 0x80 -> [h]
            _              -> (0x80 `xor` size bytes):bytes
instance SerialStream char builder stream => Format char builder stream Int where
  datum = datum >>= \(n :: Word8) ->
    if n < 0x80 then return (fromIntegral n)
    else do
      bytes <- sequence (datum <$ [1..(n`xor`0x80)])
      return $ sum (zipWith shiftL (map (fromIntegral :: Word8 -> Int) bytes) [0,8..])
instance Serializable Word8 Builder Bytes Float where encode p = encode p . floatToWord
instance Format Word8 Builder Bytes Float where datum = wordToFloat<$>datum
instance Serializable Word8 Builder Bytes Double where encode p = encode p . doubleToWord
instance Format Word8 Builder Bytes Double where datum = wordToDouble<$>datum
instance SerialStream char builder stream => Serializable char builder stream Bool where
  encode p = encodeByte p . fromIntegral . fromEnum
instance SerialStream char builder stream => Format char builder stream Bool where
  datum = toEnum . (fromIntegral :: Word8 -> Int) <$> datum

instance SerialStream char builder stream => Serializable char builder stream () where encode _ = zero
instance SerialStream char builder stream => Format char builder stream () where datum = unit
instance SerialStream char builder stream => Serializable char builder stream Integer where
  encode p n = encode p s + foldMap (encodeByte p . fromIntegral) (take s l)
    where l = iterate (`shiftR`8) (if n>=0 then n else (-n))
          s = length (takeWhile (/=0) l)
instance SerialStream char builder stream => Format char builder stream Integer where
  datum = do
    n <- datum
    doTimes n datum <&> sum . zipWith (\sh b -> fromIntegral (b :: Word8)`shiftL`sh) [0,8..]
instance Serializable char builder stream a => Serializable char builder stream (Maybe a) where
  encode p Nothing = encodeAlt p 0 ()
  encode p (Just a) = encodeAlt p 1 a
instance Format char builder stream a => Format char builder stream (Maybe a) where
  datum = datumOf [FormatAlt (\() -> Nothing), FormatAlt Just]
instance Serializable char builder stream a => Serializable char builder stream [a] where
  encode p l = encode p (length l) + foldMap (encode p) l
instance Format char builder stream a => Format char builder stream [a] where
  datum = datum >>= \n -> doTimes n datum
instance (Serializable char builder stream (f (Free f a)),Serializable char builder stream a) => Serializable char builder stream (Free f a) where
  encode p (Pure s) = encodeAlt p 0 s
  encode p (Join f) = encodeAlt p 1 f
instance (Format char builder stream (f (Free f a)),Format char builder stream a) => Format char builder stream (Free f a) where
  datum = datumOf [FormatAlt Pure,FormatAlt Join]
instance (Serializable char builder stream (f (Cofree f a)),Serializable char builder stream a) => Serializable char builder stream (Cofree f a) where
  encode p (Step a fc) = encode p (a,fc)
instance (Format char builder stream (f (Cofree f a)),Format char builder stream a) => Format char builder stream (Cofree f a) where
  datum = uncurry Step<$>datum
instance Serializable char builder stream (f (g a)) => Serializable char builder stream ((f:.:g) a) where
  encode = coerceEncode Compose
instance Format char builder stream (f (g a)) => Format char builder stream ((f:.:g) a) where
  datum = coerceDatum Compose
instance (Serializable char builder stream k,Serializable char builder stream a) => Serializable char builder stream (Map k a) where
  encode p m = encode p (m^.keyed & toList)
instance (Ord k,Format char builder stream k,Format char builder stream a) => Format char builder stream (Map k a) where
  datum = datum <&> fromAList
instance (Serializable char builder stream k,Serializable char builder stream a) => Serializable char builder stream (Bimap k a) where
  encode p m = encode p (toMap m^.keyed & toList)
instance (Ord k,Ord a,Format char builder stream k,Format char builder stream a) => Format char builder stream (Bimap k a) where
  datum = datum <&> fromAList
instance Serializable char builder stream a => Serializable char builder stream (Set a) where
  encode p = encode p . toList
instance (Ord a,Format char builder stream a) => Format char builder stream (Set a) where
  datum = datum <&> fromKList
instance (Ord a,Ord b,Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream e) => Serializable char builder stream (Relation e a b) where
  encode p = encode p . (^.i'ranges)
instance (Ord a,Ord b,Format char builder stream a,Format char builder stream b,Format char builder stream e) => Format char builder stream (Relation e a b) where
  datum = yb i'ranges <$> datum
instance (Ord a,Serializable char builder stream a,Serializable char builder stream e) => Serializable char builder stream (Equiv e a) where
  encode p (Equiv e) = encode p (toList (e^.i'domains) <&> \d -> let l = d^.ascList in (snd (head l),map fst l))
instance (Ord a,Format char builder stream a,Format char builder stream e) => Format char builder stream (Equiv e a) where
  datum = datum <&> Equiv . \l -> fromAList [(a,yb ascList (map (,e) as)) | (e,as@(a:_)) <- l]^..i'domains
instance Serializable char builder stream a => Serializable char builder stream (Range a) where
  encode = coerceEncode Range
instance Format char builder stream a => Format char builder stream (Range a) where
  datum = coerceDatum Range
instance (Serializable char builder stream a,Serializable char builder stream b) => Serializable char builder stream (a:*:b) where
  encode p (a,b) = encode p a+encode p b
instance (Format char builder stream a,Format char builder stream b) => Format char builder stream (a:*:b) where
  datum = (,)<$>datum<*>datum
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c) => Serializable char builder stream (a,b,c) where
  encode p (a,b,c) = encode p a+encode p b+encode p c
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c) => Format char builder stream (a,b,c) where
  datum = (,,)<$>datum<*>datum<*>datum
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d) => Serializable char builder stream (a,b,c,d) where
  encode p (a,b,c,d) = encode p a+encode p b+encode p c+encode p d
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d) => Format char builder stream (a,b,c,d) where
  datum = (,,,)<$>datum<*>datum<*>datum<*>datum
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d,Serializable char builder stream e) => Serializable char builder stream (a,b,c,d,e) where
  encode p (a,b,c,d,e) = encode p a+encode p b+encode p c+encode p d+encode p e
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d,Format char builder stream e) => Format char builder stream (a,b,c,d,e) where
  datum = (,,,,)<$>datum<*>datum<*>datum<*>datum<*>datum
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d,Serializable char builder stream e,Serializable char builder stream f) => Serializable char builder stream (a,b,c,d,e,f) where
  encode p (a,b,c,d,e,f) = encode p a+encode p b+encode p c+encode p d+encode p e+encode p f
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d,Format char builder stream e,Format char builder stream f) => Format char builder stream (Tuple6 a b c d e f) where
  datum = (,,,,,)<$>datum<*>datum<*>datum<*>datum<*>datum<*>datum
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d,Serializable char builder stream e,Serializable char builder stream f,Serializable char builder stream g) => Serializable char builder stream (a,b,c,d,e,f,g) where
  encode p (a,b,c,d,e,f,g) = encode p a+encode p b+encode p c+encode p d+encode p e+encode p f+encode p g
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d,Format char builder stream e,Format char builder stream f,Format char builder stream g) => Format char builder stream (Tuple7 a b c d e f g) where
  datum = (,,,,,,)<$>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d,Serializable char builder stream e,Serializable char builder stream f,Serializable char builder stream g,Serializable char builder stream h) => Serializable char builder stream (a,b,c,d,e,f,g,h) where
  encode p (a,b,c,d,e,f,g,h) = encode p a+encode p b+encode p c+encode p d+encode p e+encode p f+encode p g+encode p h
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d,Format char builder stream e,Format char builder stream f,Format char builder stream g,Format char builder stream h) => Format char builder stream (Tuple8 a b c d e f g h) where
  datum = (,,,,,,,)<$>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d,Serializable char builder stream e,Serializable char builder stream f,Serializable char builder stream g,Serializable char builder stream h,Serializable char builder stream i) => Serializable char builder stream (a,b,c,d,e,f,g,h,i) where
  encode p (a,b,c,d,e,f,g,h,i) = encode p a+encode p b+encode p c+encode p d+encode p e+encode p f+encode p g+encode p h+encode p i
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d,Format char builder stream e,Format char builder stream f,Format char builder stream g,Format char builder stream h,Format char builder stream i) => Format char builder stream (Tuple9 a b c d e f g h i) where
  datum = (,,,,,,,,)<$>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum

instance (Serializable char builder stream a,Serializable char builder stream b) => Serializable char builder stream (a:+:b) where
  encode p (Left a) = encodeAlt p 0 a
  encode p (Right a) = encodeAlt p 1 a
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c) => Serializable char builder stream (Union3 a b c) where
  encode p (U3_1 a) = encodeAlt p 0 a
  encode p (U3_2 b) = encodeAlt p 1 b
  encode p (U3_3 c) = encodeAlt p 2 c
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d) => Serializable char builder stream (Union4 a b c d) where
  encode p (U4_1 x) = encodeAlt p 0 x
  encode p (U4_2 x) = encodeAlt p 1 x
  encode p (U4_3 x) = encodeAlt p 2 x
  encode p (U4_4 x) = encodeAlt p 3 x
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d,Serializable char builder stream e) => Serializable char builder stream (Union5 a b c d e) where
  encode p (U5_1 x) = encodeAlt p 0 x
  encode p (U5_2 x) = encodeAlt p 1 x
  encode p (U5_3 x) = encodeAlt p 2 x
  encode p (U5_4 x) = encodeAlt p 3 x
  encode p (U5_5 x) = encodeAlt p 4 x
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d,Serializable char builder stream e,Serializable char builder stream f) => Serializable char builder stream (Union6 a b c d e f) where
  encode p (U6_1 x) = encodeAlt p 0 x
  encode p (U6_2 x) = encodeAlt p 1 x
  encode p (U6_3 x) = encodeAlt p 2 x
  encode p (U6_4 x) = encodeAlt p 3 x
  encode p (U6_5 x) = encodeAlt p 4 x
  encode p (U6_6 x) = encodeAlt p 5 x
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d,Serializable char builder stream e,Serializable char builder stream f,Serializable char builder stream g) => Serializable char builder stream (Union7 a b c d e f g) where
  encode p (U7_1 x) = encodeAlt p 0 x
  encode p (U7_2 x) = encodeAlt p 1 x
  encode p (U7_3 x) = encodeAlt p 2 x
  encode p (U7_4 x) = encodeAlt p 3 x
  encode p (U7_5 x) = encodeAlt p 4 x
  encode p (U7_6 x) = encodeAlt p 5 x
  encode p (U7_7 x) = encodeAlt p 6 x
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d,Serializable char builder stream e,Serializable char builder stream f,Serializable char builder stream g,Serializable char builder stream h) => Serializable char builder stream (Union8 a b c d e f g h) where
  encode p (U8_1 x) = encodeAlt p 0 x
  encode p (U8_2 x) = encodeAlt p 1 x
  encode p (U8_3 x) = encodeAlt p 2 x
  encode p (U8_4 x) = encodeAlt p 3 x
  encode p (U8_5 x) = encodeAlt p 4 x
  encode p (U8_6 x) = encodeAlt p 5 x
  encode p (U8_7 x) = encodeAlt p 6 x
  encode p (U8_8 x) = encodeAlt p 7 x
instance (Serializable char builder stream a,Serializable char builder stream b,Serializable char builder stream c,Serializable char builder stream d,Serializable char builder stream e,Serializable char builder stream f,Serializable char builder stream g,Serializable char builder stream h,Serializable char builder stream i) => Serializable char builder stream (Union9 a b c d e f g h i) where
  encode p (U9_1 x) = encodeAlt p 0 x
  encode p (U9_2 x) = encodeAlt p 1 x
  encode p (U9_3 x) = encodeAlt p 2 x
  encode p (U9_4 x) = encodeAlt p 3 x
  encode p (U9_5 x) = encodeAlt p 4 x
  encode p (U9_6 x) = encodeAlt p 5 x
  encode p (U9_7 x) = encodeAlt p 6 x
  encode p (U9_8 x) = encodeAlt p 7 x
  encode p (U9_9 x) = encodeAlt p 8 x

instance (Format char builder stream a,Format char builder stream b) => Format char builder stream (a:+:b) where
  datum = datumOf [FormatAlt Left,FormatAlt Right]
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c) => Format char builder stream (Union3 a b c) where
  datum = datumOf [FormatAlt U3_1,FormatAlt U3_2,FormatAlt U3_3]
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d) => Format char builder stream (Union4 a b c d) where
  datum = datumOf [FormatAlt U4_1,FormatAlt U4_2,FormatAlt U4_3,FormatAlt U4_4]
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d,Format char builder stream e) => Format char builder stream (Union5 a b c d e) where
  datum = datumOf [FormatAlt U5_1,FormatAlt U5_2,FormatAlt U5_3,FormatAlt U5_4,FormatAlt U5_5]
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d,Format char builder stream e,Format char builder stream f) => Format char builder stream (Union6 a b c d e f) where
  datum = datumOf [FormatAlt U6_1,FormatAlt U6_2,FormatAlt U6_3,FormatAlt U6_4,FormatAlt U6_5,FormatAlt U6_6]
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d,Format char builder stream e,Format char builder stream f,Format char builder stream g) => Format char builder stream (Union7 a b c d e f g) where
  datum = datumOf [FormatAlt U7_1,FormatAlt U7_2,FormatAlt U7_3,FormatAlt U7_4,FormatAlt U7_5,FormatAlt U7_6,FormatAlt U7_7]
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d,Format char builder stream e,Format char builder stream f,Format char builder stream g,Format char builder stream h) => Format char builder stream (Union8 a b c d e f g h) where
  datum = datumOf [FormatAlt U8_1,FormatAlt U8_2,FormatAlt U8_3,FormatAlt U8_4,FormatAlt U8_5,FormatAlt U8_6,FormatAlt U8_7,FormatAlt U8_8]
instance (Format char builder stream a,Format char builder stream b,Format char builder stream c,Format char builder stream d,Format char builder stream e,Format char builder stream f,Format char builder stream g,Format char builder stream h,Format char builder stream i) => Format char builder stream (Union9 a b c d e f g h i) where
  datum = datumOf [FormatAlt U9_1,FormatAlt U9_2,FormatAlt U9_3,FormatAlt U9_4,FormatAlt U9_5,FormatAlt U9_6,FormatAlt U9_7,FormatAlt U9_8,FormatAlt U9_9]
