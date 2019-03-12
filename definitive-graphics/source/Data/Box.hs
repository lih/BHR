module Data.Box where

import Definitive

data Box v a = Box { boxBottom, boxCenter, boxTop :: v a }

instance Functor v => Functor (Box v) where
  map f (Box b c t) = Box (map f b) (map f c) (map f t)
instance Unit v => Unit (Box v) where
  pure a = Box (pure a) (pure a) (pure a)
instance SemiApplicative v => SemiApplicative (Box v) where
  Box fb fc ft <*> Box xb xc xt = Box (fb<*>xb) (fc<*>xc) (ft<*>xt)
instance Applicative v => Applicative (Box v)
instance Foldable v => Foldable (Box v) where
  fold (Box b c t) = fold b + fold c + fold t
instance Traversable v => Traversable (Box v) where
  sequence (Box b c t) = Box<$>sequence b<*>sequence c<*>sequence t


