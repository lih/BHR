module Data.TimeVal (
  TimeVal(..)
  ) where

import Definitive

-- |A type wrapper that adds a Bounded instance for types that don't possess one.
data TimeVal t = Always | Since t | Never
                 deriving (Show,Eq,Ord)
instance Functor TimeVal where
  map f (Since a) = Since (f a)
  map _ Always = Always
  map _ Never = Never
instance Unit TimeVal where pure = Since
instance SemiApplicative TimeVal
instance Applicative TimeVal
instance Monad TimeVal where
  join (Since b) = b
  join Always = Always
  join Never = Never
instance Foldable TimeVal where
  fold (Since t) = t
  fold _ = zero
instance Traversable TimeVal where
  sequence (Since t) = Since<$>t
  sequence Always = pure Always
  sequence Never = pure Never

instance Bounded (TimeVal t) where
  minBound = Always ; maxBound = Never

data BoolNode a = Maximum a a
                | Minimum a a
                | Truth a

instance Unit BoolNode where pure = Truth
instance Functor BoolNode where
  map f (Maximum a b) = Maximum (f a) (f b)
  map f (Minimum a b) = Minimum (f a) (f b)
  map f (Truth a) = Truth (f a)
instance Foldable BoolNode where
  fold (Maximum a b) = a+b
  fold (Minimum a b) = a+b
  fold (Truth a) = a
instance Traversable BoolNode where
  sequence (Maximum fa fb) = liftA2 Maximum fa fb
  sequence (Minimum fa fb) = liftA2 Minimum fa fb
  sequence (Truth fa) = Truth<$>fa

instance Ord a => Eq (BoolNode a) where
  a == b = compare a b == EQ
instance Ord a => Ord (BoolNode a) where
  compare = cmp
    where
      cmp (Minimum a b) = cmpTo
        where cmpTo (Truth c) = scmax ac bc
                where ac = compare a c ; bc = compare b c
              cmpTo (Minimum c d) = scmin (cmpTo (Truth c)) (cmpTo (Truth d))
              cmpTo (Maximum c d) = scmax (cmpTo (Truth c)) (cmpTo (Truth d))
      cmp (Maximum a b) = cmpTo
        where cmpTo (Truth c) = scmin ac bc
                where ac = compare a c ; bc = compare b c
              cmpTo (Minimum c d) = scmin (cmpTo (Truth c)) (cmpTo (Truth d))
              cmpTo (Maximum c d) = scmax (cmpTo (Truth c)) (cmpTo (Truth d))
      cmp x = \y -> invertOrd (cmp y x)
      scmax = shortCircuit max
      scmin = shortCircuit min

shortCircuit :: (a -> a -> a) -> (a -> a -> a)
shortCircuit f = \a b -> f a b`unamb`f b a

newtype Boolean a = Boolean (Free BoolNode a)
               deriving (Eq,Ord,Functor,Foldable,Unit,SemiApplicative,Applicative)
instance Monad Boolean where join = coerceJoin Boolean
instance Traversable Boolean where sequence = coerceSequence Boolean
-- _-CONTINUE-_
