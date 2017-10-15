module Algebra.Monad.Cont (
  -- * The MonadCont class
  MonadCont(..),
  
  -- * The Continuation transformer
  ContT(..),Cont,
  contT, cont, (>>~)
  ) where

import Algebra.Monad.Base

{-| A simple continuation monad implementation  -}
newtype ContT m a = ContT { runContT :: forall r. (a -> m r) -> m r }

type Cont a = ContT Id a
instance Unit (ContT m) where pure a = ContT ($a)
instance Functor (ContT f) where
  map f (ContT c) = ContT (\kb -> c (kb . f))
instance SemiApplicative (ContT m)
instance Applicative (ContT m)
instance Monad (ContT m) where
  join (ContT kk) = ContT (\ka -> kk (\(ContT k) -> k ka))
instance MonadTrans ContT where
  lift m = ContT (m >>=)
instance MonadCont (ContT m) where
  callCC f = ContT (runContT (f pure))
instance MonadFix m => MonadFix (ContT m) where
  mfix f = ContT (\ka -> mfixing (\a -> runContT (f a) ka<&>(,a)))

(>>~) :: ContT m a -> (a -> m b) -> m b
(>>~) = runContT

contT :: (Monad m,Unit m') => Iso (ContT m a) (ContT m' a') (m a) (m' a')
contT = iso lift (>>~ pure)
cont :: Iso (Cont a) (Cont a') a a'
cont = i'Id.contT
