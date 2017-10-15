{-# LANGUAGE TupleSections, MultiParamTypeClasses #-}
module Algebra.Foldable where

import Algebra.Core hiding (flip)
import Algebra.Classes
import Algebra.Functor
import Data.Tree

instance Foldable Id where fold = getId
instance Foldable Strict where fold = lazy
instance Foldable (Either a) where
  fold = pure zero <|> id
instance Foldable Maybe where
  fold (Just w) = w ; fold Nothing = zero
instance Foldable ((,) a) where fold = snd
instance Foldable [] where
  -- | For performance reasons, we want to avoid computing (f+zero)
  -- needlessly. This cannot be inferred by the compiler, since
  -- `f+zero == f` is an implicit assumption of Monoid instances.
  -- fold [a] = a 
  fold (x:t) = x+fold t
  fold [] = zero
instance Foldable Tree where fold (Node m subs) = m + fold (map fold subs)
deriving instance Foldable Interleave
deriving instance Foldable OrdList
deriving instance Foldable (Increasing k)
instance Foldable (Assoc k) where fold (Assoc _ a) = a
instance (Foldable f,Foldable g) => Foldable (f:.:g) where
  fold = getCompose >>> map fold >>> fold

instance (Foldable f,Semigroup (f a),Ring n) => SubSemi n (f a) where
  cast = size

instance (Foldable f,Foldable g) => Foldable (f:**:g) where
  fold (f:**:g) = fold f + fold g
instance (Foldable f,Foldable g) => Foldable (f:++:g) where
  fold (Sum (Left f)) = fold f
  fold (Sum (Right g)) = fold g

instance SemiApplicative []
instance Applicative []
instance Monad [] where join = fold
instance SemiApplicative Maybe
instance Applicative Maybe
instance Monad Maybe where join = fold
instance SemiApplicative Interleave
deriving instance Unit Interleave
instance Applicative Interleave
instance Monad Interleave where join = fold

foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldMap f = fold . map f
convert :: (Unit f, Monoid (f a), Foldable t) => t a -> f a
convert = foldMap pure
concat :: (Monoid m, Foldable t) => t m -> m
concat = fold
sum :: (Monoid m, Foldable t) => t m -> m
sum = fold
product :: (Ring m,Foldable t) => t m -> m
product = getProduct . foldMap Product
nzsum :: Semigroup m => [m] -> m
nzsum = foldr1 (+)
size :: (Foldable f,Ring n) => f a -> n
size c = foldl' (+) zero (one<$c)
length :: [a] -> Int
length = size
maximum :: (Bounded a,Ord a,Foldable t) => t a -> a
maximum = getMax . foldMap Max
maximumBy :: (Ord a,Foldable t) => (b -> a) -> b -> t b -> b
maximumBy f x = foldl' g x
  where g a b = if f a > f b then a else b
minimum :: (Bounded a,Ord a,Foldable t) => t a -> a
minimum = getMax . getProduct . foldMap (Product . Max)
minimumBy :: (Ord a,Foldable t) => (b -> a) -> b -> t b -> b
minimumBy f x = foldl' g x
  where g a b = if f a < f b then a else b

sequence_ :: (Applicative f,Foldable t) => t (f a) -> f ()
sequence_ = foldr ((<*>) . map (flip const)) (pure ())
traverse_ :: (Applicative f,Foldable t) => (a -> f b) -> t a -> f ()
traverse_ f = sequence_ . map f
for_ :: (Applicative f,Foldable t) => t a -> (a -> f b) -> f ()
for_ = flip traverse_

split :: (Foldable t,Monoid b,Monoid c) => t (b:+:c) -> (b,c)
split = foldMap ((,zero)<|>(zero,))
partitionEithers :: (Foldable t,Unit t,Monoid (t a),Monoid (t b))
                    => t (a:+:b) -> (t a,t b)
partitionEithers = split . map (pure|||pure)
partition :: (Unit f, Monoid (f a), Foldable t) => (a -> Bool) -> t a -> (f a, f a)
partition p = split . map (\a -> (if p a then Left else Right) (pure a))
-- filter :: (Unit f, Monoid (f a), Foldable t) => (a -> Bool) -> t a -> f a
-- filter
select :: (Unit f, Monoid (f a), Foldable t) => (a -> Bool) -> t a -> f a
select p = fst . partition p
refuse :: (Unit f, Monoid (f a), Foldable t) => (a -> Bool) -> t a -> f a
refuse = select . map not

compose :: (Category k, Foldable t) => t (k a a) -> k a a
compose = runEndo . foldMap Endo
composing :: (Category k,Foldable t) => (a -> k b b) -> t a -> k b b
composing f = compose . map f
iter :: (Contravariant (k a),Category k,Foldable t) => k a (t (k a a) -> a)
iter = flip compose

foldr :: Foldable t => (b -> a -> a) -> a -> t b -> a
foldr f e t = (runEndo . getDual) (foldMap (\b -> Dual (Endo (f b))) t) e
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f l = foldr f (last l) (init l)
foldl' :: Foldable t => (a -> b -> a) -> a -> t b -> a
foldl' f e t = runEndo (foldMap (\b -> Endo (\a -> a`seq`f a b)) t) e
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f ~(e:t) = foldl' f e t

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p = foldMap (select p . Id)
or :: Foldable t => t Bool -> Bool
or = fold
and :: Foldable t => t Bool -> Bool
and = getProduct . fold . map Product
all :: Foldable t => (a -> Bool) -> t a -> Bool
all = map and . map
any :: Foldable t => (a -> Bool) -> t a -> Bool
any = map or . map
elem :: (Eq a,Foldable t) => a -> t a -> Bool
elem e = any (e==)

empty :: Foldable f => f a -> Bool
empty = foldr (const (const False)) True
nonempty :: Foldable f => f a -> Bool
nonempty = not . empty

intercalate :: (Monoid m,Foldable f) => m -> f m -> m
intercalate int = snd . foldr (\x ~(lst,y) -> (False,if lst then x else (x+int+y))) (True,zero)
interleave :: (Monoid m,Foldable f) => [m] -> f m -> m
interleave int = snd . foldl' (\(lst,x) y -> case lst of
                                 (m:ms) -> (ms,x+m+y)
                                 [] -> ([],x+y)) (zero:int,zero)

-- | Lazily counts the number of elements in a structure up to a certain size
sizeTo :: Foldable f => Int -> f a -> Int
sizeTo n f = foldr g (min n) f 0
  where g _ k = \s -> if s>=n then n else k (s+1)
          
