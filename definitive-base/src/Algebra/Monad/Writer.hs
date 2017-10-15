{-# LANGUAGE UndecidableInstances #-}
module Algebra.Monad.Writer (
    -- * The Writer monad
  MonadWriter(..),
  mute,intercept,intercept',eavesdrop,

  -- * The Writer transformer
  WriterT,Writer,
  writerT,writer,pureWriter,

  -- * Keeping track of where we are
  MonadCounter(..),

  -- ** Implementation
  CounterT,Counter,
  i'counterT,i'counter
  ) where

import Algebra.Monad.Base
import Algebra.Monad.RWS

instance Monoid w => MonadWriter w ((,) w) where
  tell w = (w,())
  listen m@(w,_) = (w,m)
  censor ~(w,~(a,f)) = (f w,a)
  
mute :: MonadWriter w m => m a -> m a
mute m = censor (m<&>(,const zero))
intercept :: MonadWriter w m => m a -> m (w,a)
intercept = listen >>> mute
eavesdrop :: MonadWriter w m => m a -> m w
eavesdrop = map fst . listen
intercept' :: MonadWriter w m => m a -> m w
intercept' = map fst . intercept

{-| A simple Writer monad -}
newtype WriterT w m a = WriterT (RWST Void w Void m a)
                      deriving (Unit,Functor,SemiApplicative,Applicative,MonadFix
                               ,Foldable
                               ,MonadTrans,MonadInternal,ConcreteMonad
                               ,MonadWriter w,MonadCont,MonadList)
instance (Monoid w,Monad m) => Monad (WriterT w m) where join = coerceJoin WriterT
instance Traversable m => Traversable (WriterT e m) where sequence = coerceSequence WriterT
type Writer w a = WriterT w Id a
instance (Monoid w,MonadReader r m) => MonadReader r (WriterT w m) where
  ask = ask_ ; local = local_
instance (Monoid w,MonadState r m) => MonadState r (WriterT w m) where
  get = get_ ; put = put_ ; modify = modify_
deriving instance Semigroup (m (a,Void,w)) => Semigroup (WriterT w m a)
deriving instance Monoid (m (a,Void,w)) => Monoid (WriterT w m a)
deriving instance Semiring (m (a,Void,w)) => Semiring (WriterT w m a)
deriving instance Ring (m (a,Void,w)) => Ring (WriterT w m a)
deriving instance (Monad m, Monoid w, MonadFuture n m) => MonadFuture n (WriterT w m)

writerT :: (Functor m,Functor m') => Iso (WriterT w m a) (WriterT w' m' b) (m (w,a)) (m' (w',b))
writerT = iso _writerT t'runWriterT
  where _writerT mw = WriterT (RWST (pure (mw <&> \ ~(w,a) -> (a,zero,w) )))
        t'runWriterT (WriterT (RWST m)) = m (zero,zero) <&> \ ~(a,_,w) -> (w,a)
writer :: Iso (Writer w a) (Writer w' b) (w,a) (w',b)
writer = i'Id.writerT
pureWriter :: Monoid w => Iso (w,a) (w',b) a b
pureWriter = iso (zero,) snd

{-| The canonical representsation of a WriterAcc Monad -}
newtype CounterT w acc m a = WA { runWA :: RWST () w acc m a }
                             deriving (Functor,Unit,SemiApplicative,Applicative,MonadFix,MonadTrans,ConcreteMonad)
instance (Monoid w,Monad m) => Monad (CounterT w a m) where join = coerceJoin WA
type Counter w acc a = CounterT w acc Id a

instance (Monad m,SubSemi acc w,Monoid w) => MonadWriter w (CounterT w acc m) where
  tell w = WA (tell w >> modify (+ cast w))
  listen = WA . listen . runWA
  censor (WA m) = WA $ do
    cur <- get
    (w,a) <- listen (censor m)
    put $ cur + cast w
    return a
instance (Monad m,Monoid w,SubSemi acc w) => MonadCounter w acc (CounterT w acc m) where
  getCounter = WA get
  setCounter c = WA (put c)
instance (MonadState s m,Monoid w) => MonadState s (CounterT w acc m) where
  get = WA (lift get)
  put = WA . lift . put
deriving instance (Monad m, Monoid w, MonadFuture n m) => MonadFuture n (CounterT w acc m)

i'CounterT :: Iso (CounterT w acc m a) (CounterT w' acc' m' a') (RWST () w acc m a) (RWST () w' acc' m' a')
i'CounterT = iso WA runWA
i'counterT :: (SubSemi acc w,Monoid acc',Functor m)
              => Iso (CounterT w acc m a) (CounterT w' acc' m' a') (m (a,acc,w)) (m' (a',acc',w'))
i'counterT = iso (\m (_,s) -> m <&> \(a,s',w) -> (a,s+s',w)) ($zero).i'RWST.i'CounterT
i'counter :: (SubSemi acc w,Monoid acc')
             => Iso (Counter w acc a) (Counter w' acc' a') (a,acc,w) (a',acc',w')
i'counter = i'Id.i'counterT
