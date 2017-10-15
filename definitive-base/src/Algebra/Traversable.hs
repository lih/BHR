{-# LANGUAGE ScopedTypeVariables #-}
module Algebra.Traversable(
  module Algebra.Applicative, module Algebra.Foldable,

  Traversable(..),Contravariant(..),

  traverse,for,transpose,doTimes,doTimes_,converted,folded,

  -- * Instance utilities
  coerceSequence
  ) where

import Algebra.Classes
import Algebra.Core hiding (flip,(&))
import Algebra.Applicative
import Algebra.Foldable
import Algebra.Lens
import Data.Tree
import Unsafe.Coerce

instance Traversable ((,) c) where
  sequence ~(c,m) = (,) c<$>m
instance Traversable (Either a) where
  sequence = pure . Left <|> map Right
instance Traversable [] where
  sequence (x:xs) = (:)<$>x<*>sequence xs
  sequence [] = pure []

coerceSequence :: forall f t' t a. (Applicative f,Traversable t) => (forall b. t b -> t' b) -> (t' (f a) -> f (t' a))
coerceSequence _ = unsafeCoerce (sequence :: t (f a) -> f (t a))
instance Traversable Interleave where sequence = coerceSequence Interleave
instance Traversable OrdList where sequence = coerceSequence OrdList
instance Traversable (Increasing k) where sequence = coerceSequence Increasing
instance Traversable (Assoc k) where sequence (Assoc k fa) = Assoc k<$>fa
instance Traversable f => Traversable (Zip f) where sequence = coerceSequence Zip
instance Traversable Tree where
  sequence (Node a subs) = Node<$>a<*>sequence (map sequence subs)
instance (Traversable f,Traversable g) => Traversable (f:.:g) where
  sequence = getCompose >>> map sequence >>> sequence >>> map Compose
instance (Traversable f,Traversable g) => Traversable (f:**:g) where
  sequence (f:**:g) = (:**:)<$>sequence f<*>sequence g
instance (Traversable f,Traversable g) => Traversable (f:++:g) where
  sequence (Sum (Left f)) = Sum . Left<$>sequence f
  sequence (Sum (Right g)) = Sum . Right<$>sequence g
instance Traversable Maybe where
  sequence Nothing = pure Nothing
  sequence (Just a) = Just<$>a
instance Traversable Strict where
  sequence (Strict fa) = Strict<$>fa

converted :: (Unit f,Unit g,Foldable f,Foldable g,Monoid (f a),Monoid (g b)) => Iso (f a) (f b) (g a) (g b)
converted = iso convert convert
folded :: (Unit f',Foldable f,Monoid m) => Iso m m' (f m) (f' m')
folded = iso fold pure

traverse :: (Applicative f,Traversable t) => (a -> f b) -> t a -> f (t b)
traverse f t = sequence (map f t)
for :: (Applicative f,Traversable t) => t a -> (a -> f b) -> f (t b)
for = flip traverse
doTimes :: Applicative f => Int -> f a -> f [a]
doTimes n m = sequence (m <$ [1..n])
doTimes_ :: Applicative f => Int -> f a -> f ()
doTimes_ n m = sequence_ (m <$ [1..n])
transpose :: (Applicative f,Traversable t) => t (f a) -> f (t a)
transpose = sequence

instance Compound a b [a] [b] where
  each = traverse
