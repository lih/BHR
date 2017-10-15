{-# LANGUAGE ScopedTypeVariables, CPP #-}
module Data.Containers.Sequence (
  Sequence(..),Stream(..),i'elems,take,drop,dropping,

  -- * Strict and lazy slices (bytestrings on arbitrary Storable types)
  Slice,Slices,slice,slices,i'storables,_Slices,breadth,

  V.unsafeWith,sliceElt,span,break,

  takeWhile,takeUntil,dropWhile,dropUntil,pry,

  (++)
  ) where

import Definitive.Base
import Data.Containers
import Data.Word
import qualified Data.ByteString.Lazy as Bytes
import qualified Data.ByteString as Chunk
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable as V
import Foreign.Storable (sizeOf)
import qualified Prelude as P
import Foreign.ForeignPtr (ForeignPtr,castForeignPtr)
import Unsafe.Coerce (unsafeCoerce)

class Monoid t => Sequence t where
  splitAt :: Int -> t -> (t,t)

take :: Sequence t => Int -> t -> t
take = map2 fst splitAt
drop :: Sequence t => Int -> t -> t
drop = map2 snd splitAt
dropping :: Sequence t => Int -> Lens' t t
dropping n = lens (drop n) (\x y -> take n x+y)

instance V.Storable a => Semigroup (V.Vector a) where (+) = (V.++)
instance V.Storable a => Monoid (V.Vector a) where zero = V.empty
  
instance Sequence [a] where
  splitAt n l = (h,t)
    where ~(h,t) = case (n,l) of
            (0,_) -> ([],l)
            (_,[]) -> ([],[])
            (_,(x:l')) -> let (h',t') = splitAt (n-1) l' in (x:h',t')
                  
instance Sequence Bytes where
  splitAt = Bytes.splitAt . fromIntegral
instance Sequence Chunk where
  splitAt = Chunk.splitAt . fromIntegral
instance V.Storable a => Sequence (V.Vector a) where
  splitAt = V.splitAt

class Stream c s | s -> c where
  uncons :: s -> Maybe (c,s)
  cons :: c -> s -> s
instance Stream a [a] where
  uncons [] = Nothing
  uncons (x:xs) = Just (x,xs)
  cons = (:)
instance Stream Char Chunk where
  uncons = Char8.uncons
  cons = Char8.cons
instance Stream Word8 Bytes where
  uncons = Bytes.uncons
  cons = Bytes.cons

type Slice a = V.Vector a
i'storables :: forall a b. (V.Storable a,V.Storable b) => Iso (Slice a) (Slice b) Chunk Chunk
i'storables = iso toV fromV
  where toV bs = vec
          where
            vec = V.unsafeFromForeignPtr (castForeignPtr fptr :: ForeignPtr a) (scale off) (scale len)
            (fptr, off, len) = BSI.toForeignPtr bs
            scale = (`div` sizeOf (V.head vec))
        fromV v = BSI.fromForeignPtr (castForeignPtr fptr) 0 (len * sizeOf (undefined :: b))
          where (fptr, len) = V.unsafeToForeignPtr0 v

newtype Slices a = Slices [Slice a]
                    deriving (Semigroup,Monoid)
_Slices :: Iso (Slices a) (Slices b) [Slice a] [Slice b]
_Slices = iso Slices (\(Slices cs) -> cs)
instance V.Storable a => Sequence (Slices a) where
  splitAt _ (Slices []) = zero
  splitAt n (Slices (h:t))
    | l>n = let (vh,vt) = splitAt n h in (Slices [vh],Slices (vt:t))
    | l==n = (Slices [h],Slices t)
    | otherwise = let ~(c1,c2) = splitAt (n-l) (Slices t) in (c1 & _Slices %%~ (h:),c2)
      where l = V.length h
slice :: (V.Storable a,V.Storable b) => Iso (Slice a) (Slice b) [a] [b]
slice = iso (V.unfoldr uncons) (V.foldr (:) [])

slices :: V.Storable b => Iso (Slices a) (Slices b) (Slice a) (Slice b)
slices = iso pure V.concat . _Slices

newtype PMonad m a = PMonad { runPMonad :: m a }
instance Functor m => P.Functor (PMonad m) where fmap f (PMonad m) = PMonad (map f m)
#if MIN_VERSION_base(4,8,0)
instance Applicative m => P.Applicative (PMonad m) where pure = PMonad . pure ; PMonad f<*>PMonad x = PMonad (f<*>x)
#endif
instance Monad m => P.Monad (PMonad m) where
  PMonad m >>= k = PMonad (m >>= runPMonad . k)
  return = PMonad . pure
instance V.Storable a => DataMap (Slice a) Int a where
  at i = lens (\v -> v V.!? i) (\v e -> case e of
                                   Just a -> v V.// [(i,a)]
                                   Nothing -> take i v)

sliceElt :: (V.Storable a,V.Storable b) => Fold a b (Slice a) (Slice b)
sliceElt f = V.mapM (unsafeCoerce f) <&> runPMonad

breadth :: V.Storable a => Slices a -> Int
breadth s = s^.._Slices & foldMap V.length

span :: Stream c s => (c -> Bool) -> s -> ([c],s)
span p = fix $ \f s -> (case uncons s of
                             Just (a,t) | p a -> let ~(l,t') = f t in (a:l,t')
                             _ -> ([],s))
break :: Stream c s => (c -> Bool) -> s -> ([c],s)
break = span . map not

takeWhile :: Stream c s => (c -> Bool) -> s -> [c]
takeWhile p = fst . span p
dropWhile :: Stream c s => (c -> Bool) -> s -> s
dropWhile p = snd . span p
takeUntil :: Stream c s => (c -> Bool) -> s -> [c]
takeUntil = takeWhile . map not
dropUntil :: Stream c s => (c -> Bool) -> s -> s
dropUntil = dropWhile . map not

pry :: Stream c s => Int -> s -> ([c],s)
pry 0 s = ([],s)
pry n s = case uncons s of
  Just (a,s') -> let ~(t,l') = pry (n-1) s' in (a:t,l')
  Nothing -> ([],s)

(++) :: Stream c s => [c] -> s -> s
(a:t) ++ c = cons a (t++c)
[] ++ c = c

i'elems :: (Monoid s',Stream c s,Stream c' s') => Iso [c] [c'] s s'
i'elems = iso (takeUntil (const False)) (++zero)

newtype StreamC a = StreamC (forall x. (a -> x -> x) -> x)

instance Stream a (StreamC a) where
  cons a (StreamC l) = StreamC (\c -> c a (l c))
  uncons (StreamC l) = Just (l const,l (flip const))
