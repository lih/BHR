{-# LANGUAGE ScopedTypeVariables, TypeFamilies, ExistentialQuantification, ImplicitParams, DefaultSignatures #-}
module Language.Format (
  -- * You'll need this
  module Language.Parser,
  Gen.Generic,
  
  -- * Serialization
  Serializable(..),Format(..),Builder,bytesBuilder,chunkBuilder,serialize,serial,stringBytes,
  -- ** Convenience functions
  defaultEncode,defaultDatum,
  word8,Word8,Word16,Word32,Word64,LittleEndian(..),encodeAlt,FormatAlt(..),datumOf,getChunk,
  writeSerial,readFormat,writeHSerial,readHFormat,
  -- ** Bidirectional serialization
  runConnection,runConnection_,send,receive,
  WithResponse(..),exchange,sending,

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

class Serializable t where
  encode :: t -> Builder
  default encode :: (Gen.Generic t, GenSerializable (Gen.Rep t)) => t -> Builder
  encode = defaultEncode
class Serializable t => Format t where
  datum :: Parser Bytes t
  default datum :: (Gen.Generic t, GenFormat (Gen.Rep t)) => Parser Bytes t
  datum = defaultDatum

defaultEncode :: (Gen.Generic t, GenSerializable (Gen.Rep t)) => t -> Builder
defaultEncode a = genEncode (Gen.from a)
defaultDatum :: (Gen.Generic t, GenFormat (Gen.Rep t)) => Parser Bytes t
defaultDatum = Gen.to <$> genDatum

class GenSerializable f where
  genEncode :: f a -> Builder
class GenSerializable f => GenFormat f where
  genDatum :: Parser Bytes (f a)

class GenAlt f where
  altTotal :: f a -> Int
  altTotal _ = 1
class (GenAlt f,GenSerializable f) => GenSerAlt f where
  altEncode :: f a -> Int -> Builder
  altEncode fa n = encode n + genEncode fa
class (GenSerAlt f,GenFormat f) => GenFormatAlt f where
  altDatum :: Int -> Parser Bytes (f a)
  altDatum _ = genDatum

instance GenSerializable Gen.V1 where genEncode = undefined
instance GenFormat Gen.V1 where genDatum = undefined
instance GenAlt Gen.V1
instance GenSerAlt Gen.V1
instance GenFormatAlt Gen.V1

instance GenSerializable Gen.U1 where genEncode _ = zero
instance GenFormat Gen.U1 where genDatum = pure Gen.U1
instance GenAlt Gen.U1
instance GenSerAlt Gen.U1
instance GenFormatAlt Gen.U1

instance GenAlt (Gen.K1 i c)
instance Serializable c => GenSerializable (Gen.K1 i c) where
  genEncode (Gen.K1 c) = encode c
instance Format c => GenFormat (Gen.K1 i c) where
  genDatum = Gen.K1 <$> datum
instance Serializable c => GenSerAlt (Gen.K1 i c)
instance Format c => GenFormatAlt (Gen.K1 i c)

instance GenAlt f => GenAlt (Gen.M1 i t f) where altTotal (Gen.M1 fa) = altTotal fa
instance GenSerializable f => GenSerializable (Gen.M1 i t f) where
  genEncode (Gen.M1 x) = genEncode x
instance GenFormat f => GenFormat (Gen.M1 i t f) where
  genDatum = Gen.M1 <$> genDatum
instance GenSerAlt f => GenSerAlt (Gen.M1 i t f) where
  altEncode (Gen.M1 fa) = altEncode fa
instance GenFormatAlt f => GenFormatAlt (Gen.M1 i t f) where
  altDatum n = Gen.M1 <$> altDatum n

instance GenAlt (f Gen.:*: g)
instance (GenSerializable f, GenSerializable g) => GenSerializable (f Gen.:*: g) where
  genEncode (fa Gen.:*: ga) = genEncode fa + genEncode ga
instance (GenFormat f, GenFormat g) => GenFormat (f Gen.:*: g) where
  genDatum = liftA2 (Gen.:*:) genDatum genDatum
instance (GenSerializable f,GenSerializable g) => GenSerAlt (f Gen.:*: g)
instance (GenFormat f, GenFormat g) => GenFormatAlt (f Gen.:*: g) where

instance (GenAlt f,GenAlt g) => GenAlt (f Gen.:+: g) where
  altTotal = \_ -> tot
    where tot = altTotal (undefined :: f a) + altTotal (undefined :: g a)
instance (GenSerAlt f,GenSerAlt g) => GenSerializable (f Gen.:+: g) where
  genEncode (Gen.L1 fa) = altEncode fa 0
  genEncode (Gen.R1 ga) = altEncode ga (altTotal (undefined :: f a))
instance (GenFormatAlt f,GenFormatAlt g) => GenFormat (f Gen.:+: g) where
  genDatum = datum >>= altDatum
instance (GenSerAlt f,GenSerAlt g) => GenSerAlt (f Gen.:+: g) where
  altEncode (Gen.L1 f) n = altEncode f n
  altEncode (Gen.R1 g) n = altEncode g (altTotal (undefined :: f a)+n) 
instance (GenFormatAlt f,GenFormatAlt g) => GenFormatAlt (f Gen.:+: g) where
  altDatum = \n -> if n < tot then Gen.L1 <$> altDatum n else Gen.R1 <$> altDatum (n - tot)
    where tot = altTotal (undefined :: f a)
writeHSerial :: Serializable a => Handle -> a -> IO ()
writeHSerial h a = ser`DeepSeq.deepseq`writeHBytes h ser
  where ser = serialize a
writeSerial :: Serializable a => String -> a -> IO ()
writeSerial h a = ser`DeepSeq.deepseq`writeBytes h ser
  where ser = serialize a
readHFormat :: Format a => Handle -> IO a
readHFormat h = maybe (error "Coudn't parse format") return . matches Just datum =<< readHBytes h
readFormat :: Format a => String -> IO a
readFormat f = withFile f ReadMode $ readHFormat

coerceEncode :: forall t t'. Serializable t => (t -> t') -> (t' -> Builder)
coerceEncode _ = unsafeCoerce (encode :: t -> Builder)
coerceDatum :: forall t t'. Format t => (t -> t') -> (Parser Bytes t')
coerceDatum _ = unsafeCoerce (datum :: Parser Bytes t)

serialize :: Serializable t => t -> Bytes
serialize = toLazyByteString . encode

serial :: (Format t,Serializable t') => Traversal t t' Bytes Bytes
serial = prism (datum^.from parser & \f a -> map snd (foldr (const . Right) (Left a) (f a))) (const serialize)

bytesBuilder :: Bytes:<->:Builder
bytesBuilder = iso lazyByteString toLazyByteString
chunkBuilder :: Chunk:<->:Builder
chunkBuilder = iso byteString (by chunk.toLazyByteString)

stringBytes :: String -> Bytes
stringBytes s = foldMap encode s^..bytesBuilder

encodeAlt :: Serializable a => Word8 -> a -> Builder
encodeAlt n a = word8 n + encode a
data FormatAlt a = forall b. Format b => FormatAlt (b -> a)
datumOf :: [FormatAlt a] -> Parser Bytes a
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

send :: (Serializable t, MonadIO m, ?write :: Bytes -> IO ()) =>  t -> m ()
send x = liftIO (?write (serialize x))
receive :: (Format t,Monad m) => ParserT Bytes m t
receive = generalize datum

-- | A dummy type to specify response types
data WithResponse a = WithResponse
             deriving (Show,Eq,Ord)
instance Serializable (WithResponse a) where encode WithResponse = zero
instance Format (WithResponse a) where datum = return WithResponse
exchange :: (MonadIO m,?write :: Bytes -> IO (),Format a,Serializable b) => (WithResponse a -> b) -> ParserT Bytes m a
exchange f = do
  send (f WithResponse)
  receive
sending :: (MonadIO m,?write :: Bytes -> IO (),Serializable a) => WithResponse a -> a -> m ()
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

instance Serializable Char where
  encode = charUtf8
instance Format Char where
  datum = runStreamState (gets UTF8.uncons) >>= \x -> case x of
    Just (c,t) -> c <$ runStreamState (put t)
    Nothing -> zero
instance Serializable Word8 where
  encode = word8
instance Format Word8 where
  datum = runStreamState (gets BS.uncons) >>= \x -> case x of
    Just (c,t) -> c <$ runStreamState (put t)
    Nothing -> zero
instance Serializable Bytes where
  encode b = encode (fromIntegral (BS.length b) :: Int) + b^.bytesBuilder
instance Format Bytes where
  datum = do
    sz <- datum
    x <- remaining
    let (h,t) = splitAt sz x
    h <$ runStreamState (put t)
instance Serializable Word16 where
  encode = word16BE
instance Format Word16 where
  datum = mkW16 <$> doTimes 2 datum
    where mkW16 l = sum $ zipWith shiftL (map fi l) [8,0]
          fi = fromIntegral :: Word8 -> Word16
instance Serializable (LittleEndian Word16) where
  encode = word16LE . fromLittleEndian
instance Format (LittleEndian Word16) where
  datum = LittleEndian . mkW16 <$> doTimes 2 datum
    where mkW16 l = sum $ zipWith shiftL (map fi l) [0,8]
          fi = fromIntegral :: Word8 -> Word16
instance Serializable Word32 where
  encode = word32BE
instance Format Word32 where
  datum = mkW32 <$> doTimes 4 datum
    where mkW32 l = sum $ zipWith shiftL (map fi l) [24,16,8,0]
          fi = fromIntegral :: Word8 -> Word32
instance Serializable (LittleEndian Word32) where
  encode = word32LE . fromLittleEndian
instance Format (LittleEndian Word32) where
  datum = LittleEndian . mkW32 <$> doTimes 4 datum
    where mkW32 l = sum $ zipWith shiftL (map fi l) [0,8,16,24]
          fi = fromIntegral :: Word8 -> Word32
instance Serializable Word64 where
  encode = word64BE
instance Format Word64 where
  datum = mkW64 <$> doTimes 8 datum
    where mkW64 l = sum $ zipWith shiftL (map fi l) [56,48,40,32,24,16,8,0]
          fi = fromIntegral :: Word8 -> Word64
instance Serializable (LittleEndian Word64) where
  encode = word64LE . fromLittleEndian
instance Format (LittleEndian Word64) where
  datum = LittleEndian . mkW64 <$> doTimes 8 datum
    where mkW64 l = sum $ zipWith shiftL (map fi l) [0,8,16,24,32,40,48,56]
          fi = fromIntegral :: Word8 -> Word64
instance Serializable Int where
  encode n = foldMap word8 bytes'
    where bytes = map fromIntegral $ takeWhile (>0) $ iterate (`shiftR`8) n
          bytes' = case bytes of
            []             -> [0]
            [h] | h < 0x80 -> [h]
            _              -> (0x80 `xor` size bytes):bytes
instance Format Int where
  datum = datum >>= \(n :: Word8) ->
    if n < 0x80 then return (fromIntegral n)
    else do
      bytes <- sequence (datum <$ [1..(n`xor`0x80)])
      return $ sum (zipWith shiftL (map (fromIntegral :: Word8 -> Int) bytes) [0,8..])
instance Serializable Float where encode = encode . floatToWord
instance Format Float where datum = wordToFloat<$>datum
instance Serializable Double where encode = encode . doubleToWord
instance Format Double where datum = wordToDouble<$>datum
instance Serializable Bool where
  encode = word8 . fromIntegral . fromEnum
instance Format Bool where
  datum = toEnum . (fromIntegral :: Word8 -> Int) <$> datum

instance Serializable () where encode _ = zero
instance Format () where datum = unit
instance Serializable Integer where
  encode n = encode s + foldMap (word8 . fromIntegral) (take s l)
    where l = iterate (`shiftR`8) (if n>=0 then n else (-n))
          s = length (takeWhile (/=0) l)
instance Format Integer where
  datum = do
    n <- datum
    doTimes n datum <&> sum . zipWith (\sh b -> fromIntegral (b :: Word8)`shiftL`sh) [0,8..]
instance Serializable a => Serializable (Maybe a) where
  encode Nothing = encodeAlt 0 ()
  encode (Just a) = encodeAlt 1 a
instance Format a => Format (Maybe a) where
  datum = datumOf [FormatAlt (\() -> Nothing), FormatAlt Just]
instance Serializable a => Serializable [a] where
  encode l = encode (length l) + foldMap encode l
instance Format a => Format [a] where
  datum = datum >>= \n -> doTimes n datum
instance Serializable (f (g a)) => Serializable ((f:.:g) a) where
  encode = coerceEncode Compose
instance Format (f (g a)) => Format ((f:.:g) a) where
  datum = coerceDatum Compose
instance (Serializable k,Serializable a) => Serializable (Map k a) where
  encode m = encode (m^.keyed & toList)
instance (Ord k,Format k,Format a) => Format (Map k a) where
  datum = datum <&> fromAList
instance (Serializable k,Serializable a) => Serializable (Bimap k a) where
  encode m = encode (toMap m^.keyed & toList)
instance (Ord k,Ord a,Format k,Format a) => Format (Bimap k a) where
  datum = datum <&> fromAList
instance Serializable a => Serializable (Set a) where
  encode = encode . toList
instance (Ord a,Format a) => Format (Set a) where
  datum = datum <&> fromKList
instance (Ord a,Ord b,Serializable a,Serializable b,Serializable e) => Serializable (Relation e a b) where
  encode = encode . (^.i'ranges)
instance (Ord a,Ord b,Format a,Format b,Format e) => Format (Relation e a b) where
  datum = yb i'ranges <$> datum
instance (Ord a,Serializable a,Serializable e) => Serializable (Equiv e a) where
  encode (Equiv e) = encode (toList (e^.i'domains) <&> \d -> let l = d^.ascList in (snd (head l),map fst l))
instance (Ord a,Format a,Format e) => Format (Equiv e a) where
  datum = datum <&> Equiv . \l -> fromAList [(a,yb ascList (map (,e) as)) | (e,as@(a:_)) <- l]^..i'domains
instance Serializable a => Serializable (Range a) where
  encode = coerceEncode Range
instance Format a => Format (Range a) where
  datum = coerceDatum Range
instance (Serializable a,Serializable b) => Serializable (a:*:b) where
  encode (a,b) = encode a+encode b
instance (Format a,Format b) => Format (a:*:b) where
  datum = (,)<$>datum<*>datum
instance (Serializable a,Serializable b,Serializable c) => Serializable (a,b,c) where
  encode (a,b,c) = encode a+encode b+encode c
instance (Format a,Format b,Format c) => Format (a,b,c) where
  datum = (,,)<$>datum<*>datum<*>datum
instance (Serializable a,Serializable b,Serializable c,Serializable d) => Serializable (a,b,c,d) where
  encode (a,b,c,d) = encode a+encode b+encode c+encode d
instance (Format a,Format b,Format c,Format d) => Format (a,b,c,d) where
  datum = (,,,)<$>datum<*>datum<*>datum<*>datum
instance (Serializable a,Serializable b,Serializable c,Serializable d,Serializable e) => Serializable (a,b,c,d,e) where
  encode (a,b,c,d,e) = encode a+encode b+encode c+encode d+encode e
instance (Format a,Format b,Format c,Format d,Format e) => Format (a,b,c,d,e) where
  datum = (,,,,)<$>datum<*>datum<*>datum<*>datum<*>datum
instance (Serializable a,Serializable b,Serializable c,Serializable d,Serializable e,Serializable f) => Serializable (a,b,c,d,e,f) where
  encode (a,b,c,d,e,f) = encode a+encode b+encode c+encode d+encode e+encode f
instance (Format a,Format b,Format c,Format d,Format e,Format f) => Format (Tuple6 a b c d e f) where
  datum = (,,,,,)<$>datum<*>datum<*>datum<*>datum<*>datum<*>datum
instance (Serializable a,Serializable b,Serializable c,Serializable d,Serializable e,Serializable f,Serializable g) => Serializable (a,b,c,d,e,f,g) where
  encode (a,b,c,d,e,f,g) = encode a+encode b+encode c+encode d+encode e+encode f+encode g
instance (Format a,Format b,Format c,Format d,Format e,Format f,Format g) => Format (Tuple7 a b c d e f g) where
  datum = (,,,,,,)<$>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum
instance (Serializable a,Serializable b,Serializable c,Serializable d,Serializable e,Serializable f,Serializable g,Serializable h) => Serializable (a,b,c,d,e,f,g,h) where
  encode (a,b,c,d,e,f,g,h) = encode a+encode b+encode c+encode d+encode e+encode f+encode g+encode h
instance (Format a,Format b,Format c,Format d,Format e,Format f,Format g,Format h) => Format (Tuple8 a b c d e f g h) where
  datum = (,,,,,,,)<$>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum
instance (Serializable a,Serializable b,Serializable c,Serializable d,Serializable e,Serializable f,Serializable g,Serializable h,Serializable i) => Serializable (a,b,c,d,e,f,g,h,i) where
  encode (a,b,c,d,e,f,g,h,i) = encode a+encode b+encode c+encode d+encode e+encode f+encode g+encode h+encode i
instance (Format a,Format b,Format c,Format d,Format e,Format f,Format g,Format h,Format i) => Format (Tuple9 a b c d e f g h i) where
  datum = (,,,,,,,,)<$>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum<*>datum

instance (Serializable a,Serializable b) => Serializable (a:+:b) where
  encode (Left a) = encodeAlt 0 a
  encode (Right a) = encodeAlt 1 a
instance (Serializable a,Serializable b,Serializable c) => Serializable (Union3 a b c) where
  encode (U3_1 a) = encodeAlt 0 a
  encode (U3_2 b) = encodeAlt 1 b
  encode (U3_3 c) = encodeAlt 2 c
instance (Serializable a,Serializable b,Serializable c,Serializable d) => Serializable (Union4 a b c d) where
  encode (U4_1 x) = encodeAlt 0 x
  encode (U4_2 x) = encodeAlt 1 x
  encode (U4_3 x) = encodeAlt 2 x
  encode (U4_4 x) = encodeAlt 3 x
instance (Serializable a,Serializable b,Serializable c,Serializable d,Serializable e) => Serializable (Union5 a b c d e) where
  encode (U5_1 x) = encodeAlt 0 x
  encode (U5_2 x) = encodeAlt 1 x
  encode (U5_3 x) = encodeAlt 2 x
  encode (U5_4 x) = encodeAlt 3 x
  encode (U5_5 x) = encodeAlt 4 x
instance (Serializable a,Serializable b,Serializable c,Serializable d,Serializable e,Serializable f) => Serializable (Union6 a b c d e f) where
  encode (U6_1 x) = encodeAlt 0 x
  encode (U6_2 x) = encodeAlt 1 x
  encode (U6_3 x) = encodeAlt 2 x
  encode (U6_4 x) = encodeAlt 3 x
  encode (U6_5 x) = encodeAlt 4 x
  encode (U6_6 x) = encodeAlt 5 x
instance (Serializable a,Serializable b,Serializable c,Serializable d,Serializable e,Serializable f,Serializable g) => Serializable (Union7 a b c d e f g) where
  encode (U7_1 x) = encodeAlt 0 x
  encode (U7_2 x) = encodeAlt 1 x
  encode (U7_3 x) = encodeAlt 2 x
  encode (U7_4 x) = encodeAlt 3 x
  encode (U7_5 x) = encodeAlt 4 x
  encode (U7_6 x) = encodeAlt 5 x
  encode (U7_7 x) = encodeAlt 6 x
instance (Serializable a,Serializable b,Serializable c,Serializable d,Serializable e,Serializable f,Serializable g,Serializable h) => Serializable (Union8 a b c d e f g h) where
  encode (U8_1 x) = encodeAlt 0 x
  encode (U8_2 x) = encodeAlt 1 x
  encode (U8_3 x) = encodeAlt 2 x
  encode (U8_4 x) = encodeAlt 3 x
  encode (U8_5 x) = encodeAlt 4 x
  encode (U8_6 x) = encodeAlt 5 x
  encode (U8_7 x) = encodeAlt 6 x
  encode (U8_8 x) = encodeAlt 7 x
instance (Serializable a,Serializable b,Serializable c,Serializable d,Serializable e,Serializable f,Serializable g,Serializable h,Serializable i) => Serializable (Union9 a b c d e f g h i) where
  encode (U9_1 x) = encodeAlt 0 x
  encode (U9_2 x) = encodeAlt 1 x
  encode (U9_3 x) = encodeAlt 2 x
  encode (U9_4 x) = encodeAlt 3 x
  encode (U9_5 x) = encodeAlt 4 x
  encode (U9_6 x) = encodeAlt 5 x
  encode (U9_7 x) = encodeAlt 6 x
  encode (U9_8 x) = encodeAlt 7 x
  encode (U9_9 x) = encodeAlt 8 x

instance (Format a,Format b) => Format (a:+:b) where
  datum = datumOf [FormatAlt Left,FormatAlt Right]
instance (Format a,Format b,Format c) => Format (Union3 a b c) where
  datum = datumOf [FormatAlt U3_1,FormatAlt U3_2,FormatAlt U3_3]
instance (Format a,Format b,Format c,Format d) => Format (Union4 a b c d) where
  datum = datumOf [FormatAlt U4_1,FormatAlt U4_2,FormatAlt U4_3,FormatAlt U4_4]
instance (Format a,Format b,Format c,Format d,Format e) => Format (Union5 a b c d e) where
  datum = datumOf [FormatAlt U5_1,FormatAlt U5_2,FormatAlt U5_3,FormatAlt U5_4,FormatAlt U5_5]
instance (Format a,Format b,Format c,Format d,Format e,Format f) => Format (Union6 a b c d e f) where
  datum = datumOf [FormatAlt U6_1,FormatAlt U6_2,FormatAlt U6_3,FormatAlt U6_4,FormatAlt U6_5,FormatAlt U6_6]
instance (Format a,Format b,Format c,Format d,Format e,Format f,Format g) => Format (Union7 a b c d e f g) where
  datum = datumOf [FormatAlt U7_1,FormatAlt U7_2,FormatAlt U7_3,FormatAlt U7_4,FormatAlt U7_5,FormatAlt U7_6,FormatAlt U7_7]
instance (Format a,Format b,Format c,Format d,Format e,Format f,Format g,Format h) => Format (Union8 a b c d e f g h) where
  datum = datumOf [FormatAlt U8_1,FormatAlt U8_2,FormatAlt U8_3,FormatAlt U8_4,FormatAlt U8_5,FormatAlt U8_6,FormatAlt U8_7,FormatAlt U8_8]
instance (Format a,Format b,Format c,Format d,Format e,Format f,Format g,Format h,Format i) => Format (Union9 a b c d e f g h i) where
  datum = datumOf [FormatAlt U9_1,FormatAlt U9_2,FormatAlt U9_3,FormatAlt U9_4,FormatAlt U9_5,FormatAlt U9_6,FormatAlt U9_7,FormatAlt U9_8,FormatAlt U9_9]
