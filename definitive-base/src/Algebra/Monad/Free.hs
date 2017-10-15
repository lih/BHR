{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}
module Algebra.Monad.Free where

import Algebra.Monad.Base
import Unsafe.Coerce (unsafeCoerce)

data Free f a = Join (Forest f a)
              | Pure a
type Forest f a = f (Free f a)

deriving instance (Eq (f (Free f a)),Eq a) => Eq (Free f a)
deriving instance (Ord (f (Free f a)),Ord a) => Ord (Free f a)
deriving instance (Show (f (Free f a)),Show a) => Show (Free f a)

t'Join :: Traversal (f (Free f a)) (g (Free g a)) (Free f a) (Free g a)
t'Join k (Join x) = Join<$>k x
t'Join _ (Pure a) = pure (Pure a)
t'Pure :: Traversal' (Free f a) a
t'Pure k (Pure a) = Pure<$>k a
t'Pure _ x = pure x

instance Semigroup (f (Free f a)) => Semigroup (Free f a) where
  Join a + Join b = Join (a+b)
  Join a + _ = Join a
  a + _ = a
instance Monoid (f (Free f a)) => Monoid (Free f a) where
  zero = Join zero

instance Functor f => Functor (Free f) where
  map f (Join fa) = Join (map2 f fa)
  map f (Pure a) = Pure (f a)
instance Unit (Free f) where pure = Pure
instance Functor f => SemiApplicative (Free f)
instance Functor f => Applicative (Free f)
instance Functor f => Monad (Free f) where
  join (Join f) = Join (map join f)
  join (Pure f) = f
instance Counit f => Counit (Free f) where
  extract (Join f) = extract (extract f)
  extract (Pure a) = a
instance Comonad f => Comonad (Free f) where
  duplicate (Pure a) = Pure (Pure a)
  duplicate (Join f) = Join (f =>> liftF)

instance MonadFix f => MonadFix (Free f) where
  mfix f = Join (Pure<$>mfix (\a -> perform (f a)))

instance Foldable f => Foldable (Free f) where
  fold (Join f) = foldMap fold f
  fold (Pure a) = a
instance Traversable f => Traversable (Free f) where
  sequence (Join f) = Join<$>(traverse sequence f)
  sequence (Pure a) = Pure<$>a

instance Unit (Zip (Free f)) where
  pure = Zip . Pure
instance (Functor f,SemiApplicative (Zip f)) => SemiApplicative (Zip (Free f)) where
  Zip (Join f) <*> Zip (Join x) = Zip (Join (zipWith zap f x))
  Zip (Pure f) <*> Zip x = Zip (map f x)
  Zip f <*> Zip (Pure x) = Zip (map ($x) f)
instance (Functor f,Applicative (Zip f)) => Applicative (Zip (Free f)) where
         
instance MonadTrans Free where lift = liftF
instance ConcreteMonad Free where
  generalize (Join f) = Join ((pure . generalize . getId) f)
  generalize (Pure a) = Pure a
instance MonadState s m => MonadState s (Free m) where
  get = lift get
  put a = lift (put a)
  modify f = lift (modify f)
instance MonadReader r m => MonadReader r (Free m) where
  ask = lift ask
  local f (Join m) = Join (local f m)
  local _ (Pure a) = Pure a
instance MonadWriter w m => MonadWriter w (Free m) where
  tell w = lift (tell w)
  listen m = lift (listen (perform m))
  censor m = lift (censor (perform m))
instance MonadCounter w a m => MonadCounter w a (Free m) where
  getCounter = lift getCounter ; setCounter c = lift (setCounter c)
instance MonadIO m => MonadIO (Free m) where
  liftIO = lift . liftIO
instance MonadList m => MonadList (Free m) where
  fork l = lift (fork l)
instance MonadFuture m t => MonadFuture m (Free t) where
  future = lift . future

instance MonadError e m => MonadError e (Free m) where
  throw e = lift (throw e)
  catch k m = lift (catch (map perform k) (perform m))

concrete :: Monad m => Free m a -> m (Free Id a)
concrete = map Pure . perform 
unliftF :: Monad m => Free m a -> Free m (m a)
unliftF = Pure . perform

mapF :: (Functor f,Functor g) => (forall a. f a -> g a) -> Free f b -> Free g b
mapF f (Join a) = Join (f (map (mapF f) a))
mapF _ (Pure a) = Pure a
sequenceF :: (Traversable f,Monad g) => Free (g:.:f) a -> g (Free f a)
sequenceF (Join (Compose gfa)) = map Join (gfa >>= \fa -> traverse sequenceF fa)
sequenceF (Pure a) = pure (Pure a)
traverseF :: (Functor f,Traversable f',Monad g) => (forall a. f a -> g (f' a)) -> Free f b -> g (Free f' b)
traverseF f = sequenceF . mapF (\fa -> Compose (f fa))

class MonadFree m f | f -> m where
  step :: Monad m => f a -> m (f a)
  perform :: Monad m => f a -> m a
  liftF :: Functor m => m a -> f a

instance MonadFree m (Free m) where
  step (Join j) = j
  step (Pure a) = pure (Pure a)
  perform (Join fa) = fa >>= perform
  perform (Pure a) = pure a
  liftF = Join . map Pure
coerceStep :: forall m f g a. (Monad m,MonadFree m f) => (f a -> g a) -> (g a -> m (g a))
coerceStep _ = unsafeCoerce (step :: f a -> m (f a))
coercePerform :: forall m f g a. (Monad m,MonadFree m f) => (f a -> g a) -> (g a -> m a)
coercePerform _ = unsafeCoerce (perform :: f a -> m a)
coerceLiftF :: forall m f g a. (Functor m,MonadFree m f) => (f a -> g a) -> (m a -> g a)
coerceLiftF _ = unsafeCoerce (liftF :: m a -> f a)

data Cofree w a = Step a (Coforest w a)
type Coforest w a = w (Cofree w a)

type Infinite a = Cofree Id a
type Colist a = Cofree Maybe a

instance Functor w => Functor (Cofree w) where
  map f (Step a wca) = Step (f a) (map2 f wca)
instance Counit (Cofree w) where
  extract (Step a _) = a
instance Functor w => Comonad (Cofree w) where
  duplicate d@(Step _ wca) = Step d (map duplicate wca)
instance Foldable w => Foldable (Cofree w) where
  fold (Step a wca) = a + foldMap fold wca
instance Traversable w => Traversable (Cofree w) where
  sequence (Step fa wcfa) = Step<$>fa<*>traverse sequence wcfa
instance Unit m => Unit (Cofree m) where
  pure a = Step a (pure (pure a))
instance Applicative m => SemiApplicative (Cofree m)
instance Applicative m => Applicative (Cofree m)
instance Applicative m => Monad (Cofree m) where
  join (Step (Step a _) ww) = Step a (map join ww)

type Bifree f a = Cofree (Free f) a
newtype ContC k a b = ContC { runContC :: forall c. k b c -> k a c }
contC :: (Category k,Category k') => Iso (ContC k a b) (ContC k' a' b') (k a b) (k' a' b')
contC = iso (\x -> ContC (x >>>)) (($id) . runContC)

instance Deductive (ContC k) where
  ContC cxbx . ContC bxax = ContC (\kcx -> bxax (cxbx kcx))
instance Category (ContC k) where
  id = ContC id
