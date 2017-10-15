{-# LANGUAGE DefaultSignatures, TupleSections #-}
module Algebra.Arrow (
  module Algebra.Monad,
 
  Arrow(..),
  (>>^),(^>>),

  Apply(..),comapA,app,dup,

  Kleisli(..),

  ListA(..)
  ) where

import Algebra.Core hiding (flip)
import Algebra.Classes
import Algebra.Monad

comapA :: Arrow arr => (a -> b) -> Flip arr c b -> Flip arr c a
app :: Apply k => k a b -> k a b

(^>>) :: Cofunctor (Flip f c) => (a -> b) -> f b c -> f a c
(>>^) :: Functor f => f a -> (a -> b) -> f b
dup :: Arrow arr => arr a (a, a)

class (Split k,Choice k) => Arrow k where
  arr :: (a -> b) -> k a b
instance Arrow (->) where arr = id
instance Monad m => Arrow (StateA m) where
  arr f = StateA (f<$>get)

class Arrow k => Apply k where
  apply :: k (k a b,a) b
instance Apply (->) where apply (f,x) = f x

instance Monad m => Apply (Kleisli m) where
  apply = Kleisli (\(Kleisli f,a) -> f a)
instance Monad m => Arrow (Kleisli m) where
  arr a = Kleisli (pure . a)

newtype ListA k a b = ListA { runListA :: k [a] [b] }
instance Deductive k => Deductive (ListA k) where
  ListA a . ListA b = ListA (a . b)
instance Category k => Category (ListA k) where
  id = ListA id
instance Arrow k => Choice (ListA k) where
  ListA f <|> ListA g = ListA (arr partitionEithers >>> (f<#>g) >>> arr (uncurry (+)))
instance Arrow k => Split (ListA k) where
  ListA f <#> ListA g = ListA (arr (\l -> (fst<$>l,snd<$>l)) >>> (f<#>g)
                               >>> arr (\(c,d) -> (,)<$>c<*>d))
instance Arrow k => Arrow (ListA k) where
  arr f = ListA (arr (map f))

(^>>) = promap
(>>^) = (<&>)
infixr 4 ^>>,>>^
dup = arr (\a -> (a,a))

comapA f (Flip g) = Flip (arr f >>> g)
app f = arr (f,) >>> apply
