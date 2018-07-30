{-# LANGUAGE UndecidableInstances #-}
module Algebra.Monad.Reader (
  -- *** The Reader monad
  MonadReader(..),
  ReaderT,Reader,
  readerT,reader,
  ) where

import Algebra.Monad.Base
import Algebra.Monad.RWS

instance MonadReader r ((->) r) where
  ask = id ; local = (>>>)

{-| A simple Reader monad -}
newtype ReaderT r m a = ReaderT (RWST r Void Void m a) 
                      deriving (Functor,Unit,SemiApplicative,Applicative,MonadFix,
                                MonadTrans,MonadInternal,
                                MonadReader r,MonadCont,MonadList)
instance Monad m => Monad (ReaderT r m) where join = coerceJoin ReaderT
type Reader r a = ReaderT r Id a

instance MonadState s m => MonadState s (ReaderT r m) where
  get = get_ ; put = put_ ; modify = modify_
instance MonadWriter w m => MonadWriter w (ReaderT r m) where
  tell = tell_ ; listen = listen_ ; censor = censor_
instance MonadCounter w acc m => MonadCounter w acc (ReaderT r m) where
  getCounter = getCounter_ ; setCounter = setCounter_
deriving instance Semigroup (m (a,Void,Void)) => Semigroup (ReaderT r m a)
deriving instance Monoid (m (a,Void,Void)) => Monoid (ReaderT r m a)
deriving instance Semiring (m (a,Void,Void)) => Semiring (ReaderT r m a)
deriving instance Ring (m (a,Void,Void)) => Ring (ReaderT r m a)
deriving instance (Monad m,MonadFuture n m) => MonadFuture n (ReaderT r m)
instance MonadLogic m l => MonadLogic (ReaderT r m) (ReaderT r l) where
  deduce = coerceDeduce ReaderT ReaderT
  induce = coerceInduce ReaderT ReaderT

readerT :: (Functor m,Functor m') => Iso (ReaderT r m a) (ReaderT r' m' b) (r -> m a) (r' -> m' b)
readerT = iso t'readerT t'runReaderT
  where t'readerT f = ReaderT (RWST (\ ~(r,_) -> f r<&>(,zero,zero) ))
        t'runReaderT (ReaderT (RWST f)) r = f (r,zero) <&> \ ~(a,_,_) -> a
reader :: Iso (Reader r a) (Reader r' b) (r -> a) (r' -> b)
reader = mapping i'Id.readerT

