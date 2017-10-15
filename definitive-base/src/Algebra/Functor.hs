{-# LANGUAGE MultiParamTypeClasses, RankNTypes, DefaultSignatures #-}
-- |A module for functors
module Algebra.Functor(
  Functor(..),Cofunctor(..),Bifunctor(..),Commutative(..),Contravariant(..),
  
  Strict(..),Id(..),Const(..),Flip(..),(:.:)(..),(:**:)(..),(:++:)(..),
  Increasing(..),
  
  emerge,flip,project,factor,
  (<$>),(|||),(<$),(<&>),void,left,right,
  promap,fill,map2,map3,map4
  ) where

import qualified Prelude as P

import Algebra.Classes
import Algebra.Core hiding (flip)
import Data.Tree

class Cofunctor f where
  comap :: (a -> b) -> f b -> f a
instance (Functor f,Cofunctor g) => Cofunctor (f:.:g) where
  comap f (Compose c) = Compose (map (comap f) c)
instance Cofunctor (Flip (->) a) where
  comap f (Flip g) = Flip (g . f)
instance Bifunctor (->)

class Functor t => Contravariant t where
  collect :: Functor f => f (t a) -> t (f a)
instance Contravariant Id where collect f = Id (map getId f)
instance Contravariant Strict where collect f = Strict (map lazy f)
instance Contravariant ((->) a) where collect f = \a -> map ($a) f
flip :: (Contravariant c,Functor f) => f (c a) -> c (f a)
flip = collect
-- | The Contravariant version of 'traverse'
project :: (Contravariant c,Functor f) => (a -> c b) -> f a -> c (f b)
project f x = collect (map f x)
factor :: (Contravariant c,Unit c,Bifunctor f,Functor (f a)) => f (c a) (c b) -> c (f a b)
factor = collect . dimap pure id

class Bifunctor p where
  dimap :: (c -> a) -> (b -> d) -> p a b -> p c d
  default dimap :: (Functor (p a),Cofunctor (Flip p d)) => (c -> a) -> (b -> d) -> p a b -> p c d
  dimap f g = promap f . map g

class Commutative f where
  commute :: f a b -> f b a
instance Commutative (,) where
  commute (a,b) = (b,a)

instance Functor [] where map f = f' where f' [] = [] ; f' (x:t) = f x:f' t
instance Functor Tree where
  map f (Node a subs) = Node (f a) (map2 f subs)

instance Functor Id where map f (Id a) = Id (f a)
instance SemiApplicative Id
instance Applicative Id
instance Monad Id where join (Id a) = a
instance MonadFix Id where mfix f = let ret@(Id a) = f a in ret

newtype Strict a = Strict { lazy :: a }
instance Unit Strict where pure = Strict
instance Functor Strict where map f (Strict a) = Strict (f$!a)
instance SemiApplicative Strict
instance Applicative Strict
instance Monad Strict where join = lazy

-- |The Constant Functor
newtype Const a b = Const { getConst :: a }
instance Semigroup a => Semigroup (Const a b) where Const a+Const b = Const (a+b)
instance Monoid a => Monoid (Const a b) where zero = Const zero
instance Functor (Const a) where map _ (Const a) = Const a
instance Monoid a => Unit (Const a) where pure _ = Const zero
instance Monoid a => SemiApplicative (Const a) where
  Const a <*> Const b = Const (a+b)
instance Monoid a => Applicative (Const a)

-- |A functor for ordered lists
newtype Increasing k a = Increasing ((OrdList:.:Assoc k) a)
                      deriving Functor
instance Functor (Assoc k) where map f (Assoc k a) = Assoc k (f a)
instance Ord k => Semigroup (Increasing k a) where
  Increasing (Compose l) + Increasing (Compose l') = Increasing (Compose (l+l'))
instance Ord k => Monoid (Increasing k a) where
  zero = Increasing (Compose zero)

-- |A motherflippin' functor
newtype Flip f a b = Flip { unFlip :: f b a }
                  deriving (Semigroup,Monoid)

-- |The Composition functor
newtype (f:.:g) a = Compose { getCompose :: f (g a) }
                  deriving (Eq,Ord)
instance (Unit f,Unit g) => Unit (f:.:g) where pure = Compose . pure . pure
instance (Functor f,Functor g) => Functor (f:.:g) where
  map f (Compose c) = Compose (map2 f c)
instance (Contravariant f,Contravariant g) => Contravariant (f:.:g) where
  collect = Compose . map collect . collect . map getCompose

emerge :: (Functor f,Unit g) => (f:.:g) a -> (f:.:g) (g a)
emerge (Compose fga) = Compose (map pure fga)

data (f:**:g) a = f a:**:g a
instance (Functor f,Functor g) => Functor (f:**:g) where
  map f (a:**:b) = map f a:**:map f b
newtype (f:++:g) a = Sum { getSum :: f a:+:g a }
instance (Functor f,Functor g) => Functor (f:++:g) where
  map f = Sum . (map f ||| map f) . getSum

instance Functor (Either b) where map f = Left <|> Right . f
instance Functor Maybe where map _ Nothing = Nothing; map f (Just a) = Just (f a)
instance Functor ((,) b) where map f ~(b,a) = (b,f a)
instance Functor ((->) a) where map = (.)
deriving instance Functor Interleave
deriving instance Functor OrdList

instance Functor IO where map = P.fmap
instance SemiApplicative IO
instance Applicative IO
instance Monad IO where (>>=) = (P.>>=)

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = map
(|||) :: (Choice k, Functor (k a), Functor (k b)) => k a c -> k b d -> k (a:+:b) (c:+:d)
f ||| g = Left<$>f <|> Right<$>g
(<&>) :: Functor f => f a -> (a -> b) -> f b
x<&>f = map f x
fill :: Functor f => b -> f a -> f b
fill a x = const a <$> x
(<$) :: Functor f => b -> f a -> f b
(<$) = fill
infixr 2 <$>,<$
infixl 1 <&>
infixr 1 |||

left :: (Choice k, Functor (k a), Functor (k c)) => k a b -> k (a:+:c) (b:+:c)
left a = a ||| id
right :: (Choice k, Functor (k a), Functor (k c)) => k a b -> k (c:+:a) (c:+:b)
right a = id ||| a

void :: Functor f => f a -> f ()
void = (()<$)

map2 :: (Functor f, Functor f') => (a -> b) -> f (f' a) -> f (f' b)
map2 = map map map
map3 :: (Functor f, Functor f', Functor f'') => (a -> b) -> f (f' (f'' a)) -> f (f' (f'' b))
map3 = map map map2
map4 :: (Functor f, Functor f', Functor f'',Functor f''') => (a -> b) -> f (f' (f'' (f''' a))) -> f (f' (f'' (f''' b)))
map4 = map map map3

promap :: Cofunctor (Flip f c) => (a -> b) -> f b c -> f a c
promap f c = unFlip (comap f (Flip c))
