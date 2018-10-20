-- |A module describing applicative functors
{-# LANGUAGE UndecidableInstances #-}
module Algebra.Applicative(
  module Algebra.Functor,

  SemiApplicative(..),Applicative,
  Zip(..),Backwards(..),
  c'zip,c'backwards,

  (*>),(<*),(<**>),ap,

  between,
  
  liftA,liftA2,liftA3,liftA4,forever,
  zap,zap3,zipWith,zipWith3,
  
  plusA,zeroA
  ) where

import Algebra.Functor
import Algebra.Classes
import Algebra.Core hiding (flip)
import Data.Tree

instance SemiApplicative (Either a)
instance Applicative (Either a)
instance Monad (Either a) where join (Right a) = a
                                join (Left a) = Left a
instance SemiApplicative ((->) a)
instance Applicative ((->) a)
instance Semigroup b => Semigroup (a -> b) where (+) = plusA
instance Monoid b => Monoid (a -> b) where zero = zeroA
instance Semiring b => Semiring (a -> b) where (*) = timesA
instance Ring b => Ring (a -> b) where one = oneA
instance Monad ((->) a) where join f x = f x x
instance Monoid w => SemiApplicative ((,) w)
instance Monoid w => Applicative ((,) w)
instance Monoid w => Monad ((,) w) where
  join ~(w,~(w',a)) = (w+w',a)
instance Monoid k => Unit (Assoc k) where pure = Assoc zero
instance (Monoid k,Ord k) => SemiApplicative (Increasing k)
deriving instance Monoid k => Unit (Increasing k)
instance (Monoid k,Ord k) => Applicative (Increasing k)
instance (Monoid k,Ord k) => Monad (Increasing k) where
  join l = Increasing (Compose (OrdList (join' $ fromAscList (map fromAscList l))))
    where join' (Assoc k (Assoc k' a:as):ass) = Assoc (k+k') a:join' (insert (Assoc k' as) ass)
          join' (Assoc _ []:ass) = join' ass
          join' [] = []
          insert x [] = [x]
          insert x (a:as) | x<=a = x:a:as
                          | otherwise = a:insert x as
          fromAscList (Increasing (Compose (OrdList l'))) = l'

instance (Unit f,Unit g) => Unit (f:**:g) where pure a = pure a:**:pure a
instance (SemiApplicative f,SemiApplicative g) => SemiApplicative (f:**:g) where
  ff:**:fg <*> xf:**:xg = (ff<*>xf) :**: (fg<*>xg)
instance (Applicative f,Applicative g) => Applicative (f:**:g)

instance SemiApplicative Tree
instance Applicative Tree
instance Monad Tree where
  join (Node (Node a subs) subs') = Node a (subs + map join subs')

instance (SemiApplicative f,SemiApplicative g) => SemiApplicative (f:.:g) where
  Compose fs <*> Compose xs = Compose ((<*>)<$>fs<*>xs)
instance (Applicative f,Applicative g) => Applicative (f:.:g) where

{-|
A wrapper type for lists with zipping Applicative instances, such that
@Zip [f1,...,fn] '<*>' Zip [x1,...,xn] == Zip [f1 x1,...,fn xn]@
-}
newtype Zip f a = Zip { deZip :: f a }
c'zip :: Constraint (f a) -> Constraint (Zip f a)
c'zip _ = c'_

zap :: SemiApplicative (Zip f) => f (a -> b) -> f a -> f b
zap f x = deZip (Zip f<*>Zip x)
zap3 :: SemiApplicative (Zip f) => f (a -> b -> c) -> f a -> f b -> f c
zap3 f x y = deZip (Zip f<*>Zip x<*>Zip y)
zipWith :: (Functor f,SemiApplicative (Zip f)) => (a -> b -> c) -> f a -> f b -> f c
zipWith f a = zap (f<$>a)
zipWith3 :: (Functor f,SemiApplicative (Zip f)) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
zipWith3 f a = zap3 (f<$>a)

instance (SemiApplicative (Zip f),Semigroup a) => Semigroup (Zip f a) where (+) = plusA
instance (Applicative (Zip f),Monoid a) => Monoid (Zip f a) where zero = zeroA

instance Functor f => Functor (Zip f) where
  map f (Zip l) = Zip (map f l)
deriving instance Foldable f => Foldable (Zip f)

instance Unit (Zip []) where
  pure a = Zip (repeat a)
instance SemiApplicative (Zip []) where
  Zip zf <*> Zip zx = Zip (zip_ zf zx)
    where zip_ (f:fs) (x:xs) = f x:zip_ fs xs
          zip_ _ _ = []
instance Applicative (Zip []) where

instance Unit (Zip Maybe) where
  pure a = Zip (Just a)
instance SemiApplicative (Zip Maybe) where
  Zip (Just zf) <*> Zip (Just zx) = Zip (Just (zf zx))
  _ <*> _ = Zip Nothing
instance Applicative (Zip Maybe)
  
instance Unit (Zip Tree) where
  pure a = Zip (Node a (deZip (pure (pure a))))
instance SemiApplicative (Zip Tree) where
  Zip (Node f fs) <*> Zip (Node x xs) =
    Zip (Node (f x) (zipWith (<*>) fs xs))
instance Applicative (Zip Tree) where

-- |A wrapper for applicative functors with actions executed in the reverse order
newtype Backwards f a = Backwards { forwards :: f a }
c'backwards :: Constraint (f a) -> Constraint (Backwards f a)
c'backwards _ = c'_

deriving instance Semigroup (f a) => Semigroup (Backwards f a)
deriving instance Monoid (f a) => Monoid (Backwards f a)
deriving instance Semiring (f a) => Semiring (Backwards f a)
deriving instance Ring (f a) => Ring (Backwards f a)
deriving instance Unit f => Unit (Backwards f)
deriving instance Functor f => Functor (Backwards f)
instance SemiApplicative f => SemiApplicative (Backwards f) where
  Backwards fs <*> Backwards xs = Backwards (fs<**>xs)
instance Applicative f => Applicative (Backwards f) where

ap :: Applicative f => f (a -> b) -> f a -> f b

plusA :: (SemiApplicative f,Semigroup a) => f a -> f a -> f a
zeroA :: (Unit f,Monoid a) => f a
oneA :: (Unit f,Ring a) => f a
timesA :: (SemiApplicative f,Semiring a) => f a -> f a -> f a

(*>) :: SemiApplicative f => f b -> f a -> f a
(<*) :: SemiApplicative f => f a -> f b -> f a
(<**>) :: SemiApplicative f => f (a -> b) -> f a -> f b

ap = (<*>)
infixl 1 <**>
infixl 3 <*,*>
(*>) = liftA2 (flip const)
(<*) = liftA2 const
f <**> x = liftA2 (&) x f

forever :: SemiApplicative f => f a -> f b
forever m = fix (m *>)

liftA :: Functor f => (a -> b) -> (f a -> f b)
liftA = map
liftA2 :: SemiApplicative f => (a -> b -> c) -> (f a -> f b -> f c)
liftA2 f = \a b -> f<$>a<*>b
liftA3 :: SemiApplicative f => (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
liftA3 f = \a b c -> f<$>a<*>b<*>c
liftA4 :: SemiApplicative f => (a -> b -> c -> d -> e) -> (f a -> f b -> f c -> f d -> f e)
liftA4 f = \a b c d -> f<$>a<*>b<*>c<*>d

plusA = liftA2 (+)
zeroA = pure zero
oneA = pure one
timesA = liftA2 (*)

between :: SemiApplicative f => f b -> f c -> f a -> f a
between start end p = liftA3 (\_ b _ -> b) start p end

instance (SemiApplicative f,Semigroup (g a)) => Semigroup ((f:.:g) a) where
  Compose f+Compose g = Compose (plusA f g)
instance (Applicative f,Monoid (g a)) => Monoid ((f:.:g) a) where
  zero = Compose zeroA
