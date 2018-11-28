{-# LANGUAGE DefaultSignatures, ScopedTypeVariables, CPP #-}
#if MIN_VERSION_base(4,8,0)
{-# LANGUAGE TypeFamilies #-}
#endif
module Algebra.Classes where

import Algebra.Core
import Unsafe.Coerce (unsafeCoerce)

class Functor f where
  map :: (a -> b) -> f a -> f b
class Functor f => SemiApplicative f where 
  infixl 1 <*>
  (<*>) :: f (a -> b) -> f a -> f b
  default (<*>) :: Monad f => f (a -> b) -> f a -> f b
  fs <*> xs = fs >>= \f -> map f xs
class (Unit f, SemiApplicative f) => Applicative f
class Applicative m => Monad m where
  join :: m (m a) -> m a
  join m = m >>= id
  infixl 1 >>=
  (>>=) :: m a -> (a -> m b) -> m b
  ma >>= k = join (map k ma)
class Counit w where 
  extract :: w a -> a
class (Functor w,Counit w) => Comonad w where
  duplicate :: w a -> w (w a)
  duplicate = (=>> id)
  infixl 1 =>>
  (=>>) :: w a -> (w a -> b) -> w b
  wa =>> k = map k (duplicate wa)
  
class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
class Functor t => Traversable t where
  sequence :: Applicative f => t (f a) -> f (t a)

-- |The class of all monads that have a fixpoint
class Monad m => MonadFix m where
  mfix :: (a -> m a) -> m a
class MonadTrans t where
  lift :: Monad m => m a -> t m a
class MonadTrans t => ConcreteMonad t where
  generalize :: Monad m => t Id a -> t m a

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()
  put = modify . const
  modify :: (s -> s) -> m ()
  modify f = get >>= put . f
class Monad m => MonadReader r m | m -> r where
  ask :: m r
  local :: (r -> r) -> m a -> m a
class (Monad m,Monoid w) => MonadWriter w m | m -> w where
  tell :: w -> m ()
  listen :: m a -> m (w,a)
  censor :: m (a,w -> w) -> m a
class (SubSemi acc w,MonadWriter w m) => MonadCounter w acc m | m -> acc where
  getCounter :: m acc
  setCounter :: acc -> m ()
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
  default liftIO :: (MonadTrans t,MonadIO n,t n a ~ m a) => IO a -> m a
  liftIO = lift . liftIO

class Monad m => MonadList m where
  choose :: [a] -> m a
class Monad m => MonadCont m where
  callCC :: (forall b. (a -> m b) -> m b) -> m a
class Monad m => MonadError e m | m -> e where
  throw :: e -> m a
  catch :: (e -> m a) -> m a -> m a

class Monad m => MonadLogic m l | l -> m where
  deduce :: l a -> m (Maybe (a,l a))
  induce :: m (Maybe (a,l a)) -> l a
coerceDeduce :: forall l m l' m' a. MonadLogic m l => (forall b. l b -> l' b) -> (forall b. m b -> m' b) -> l' a -> m' (Maybe (a,l' a))
coerceDeduce _ _ = unsafeCoerce (deduce :: l a -> m (Maybe (a,l a)))
coerceInduce :: forall l m l' m' a. MonadLogic m l => (forall b. l b -> l' b) -> (forall b. m b -> m' b) -> m' (Maybe (a,l' a)) -> l' a
coerceInduce _ _ = unsafeCoerce (induce :: m (Maybe (a,l a)) -> l a)

class MonadFix t => MonadFuture m t | t -> m where
  future :: m a -> t a
