{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}
module Algebra.Monad.Base (
  module Algebra.Classes,module Algebra.Applicative,module Algebra.Core,
  module Algebra.Traversable,module Algebra.Lens,
  
  -- * Monad utilities
  Kleisli(..),i'Kleisli,
  (=<<),joinMap,(<=<),(>=>),(>>),(<*=),only,return,
  foldlM,foldrM,findM,while,until,
  bind2,bind3,(>>>=),(>>>>=),
  mfix_,mfixing,
  
  -- * Instance utilities
  Compose'(..),i'Compose',coerceJoin,coerceDuplicate
  ) where

import Algebra.Classes
import Algebra.Applicative
import Algebra.Core hiding (flip)
import Algebra.Traversable
import Algebra.Lens
import qualified Control.Monad.Fix as Fix
import Unsafe.Coerce (unsafeCoerce)

instance MonadIO IO where liftIO = id
instance (MonadIO m,MonadTrans t,Monad (t m)) => MonadIO (t m) where
  liftIO = lift . liftIO

-- MonadFix instances
instance MonadFix Strict where mfix = cfix
instance MonadFix ((->) b) where mfix = cfix
instance MonadFix [] where mfix f = fix (f . head)
instance MonadFix (Either e) where mfix f = fix (f . either undefined id)
instance MonadFix IO where mfix = Fix.mfix
instance Monoid b => MonadFix ((,) b) where mfix f = fix (f . snd)
mfix_ :: MonadFix m => (a -> m a) -> m ()
mfix_ = void . mfix

instance (Traversable g,Monad f,Monad g) => Monad (f:.:g) where
  join = Compose .map join.join.map sequence.getCompose.map getCompose
instance (MonadFix f,Traversable g,Monad g) => MonadFix (f:.:g) where
  mfix f = Compose $ mfix (map join . traverse (getCompose . f))
instance Monad m => MonadTrans ((:.:) m) where
  lift = Compose . pure
instance Monad m => ConcreteMonad ((:.:) m) where
  generalize = i'Compose %%~ map (pure.yb i'Id)
instance (Traversable g,Monad g,MonadState s f) => MonadState s (f:.:g) where
  get = Compose (pure<$>get)
  put x = Compose (pure<$>put x)
  modify f = Compose (pure<$>modify f)
instance (Traversable g,Monad g,MonadWriter w f) => MonadWriter w (f:.:g) where
  tell w = Compose (pure<$>tell w)
  listen (Compose fga) = Compose (listen fga <&> (\ (w,ga) -> (w, )<$>ga))
  censor (Compose fgc) = Compose (censor $ map (swap . first runEndo . sequence . map (first Endo . swap)) fgc)
instance (Traversable g,Monad g,MonadReader r f) => MonadReader r (f:.:g) where
  ask = Compose (pure<$>ask)
  local f (Compose fga) = Compose (local f fga)

instance MonadFix m => Monad (Backwards m) where
  join (Backwards ma) = Backwards$mfixing (\a -> liftA2 (,) (forwards a) ma)
instance MonadFix m => MonadFuture m (Backwards m) where
  future = Backwards
instance MonadFix m => MonadFix (Backwards m) where
  mfix f = by i'Backwards $ mfix (yb i'Backwards.f)
instance MonadTrans Backwards where
  lift = Backwards
instance ConcreteMonad Backwards where
  generalize = i'Backwards %%~ pure.yb i'Id

newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
instance Functor f => Functor (Kleisli f a) where
  map f (Kleisli k) = Kleisli (map2 f k)
instance Contravariant f => Contravariant (Kleisli f a) where
  collect f = Kleisli (\a -> project (($a) . runKleisli) f)
instance Monad m => Deductive (Kleisli m) where
  Kleisli f . Kleisli g = Kleisli (\a -> g a >>= f)
instance Monad m => Category (Kleisli m) where
  id = Kleisli pure
instance Monad m => Choice (Kleisli m) where
  Kleisli f <|> Kleisli g = Kleisli (f <|> g)
instance Monad m => Split (Kleisli m) where
  Kleisli f <#> Kleisli g = Kleisli (\(a,c) -> (,)<$>f a<*>g c)
instance Isomorphic (a -> m b) (c -> m' d) (Kleisli m a b) (Kleisli m' c d) where
  i'_ = iso Kleisli runKleisli

cfix :: Contravariant c => (a -> c a) -> c a
cfix = map fix . collect

mfixing :: MonadFix f => (b -> f (a, b)) -> f a
mfixing f = fst<$>mfix (\ ~(_,b) -> f b )

i'Kleisli :: Iso (Kleisli m a b) (Kleisli m' c d) (a -> m b) (c -> m' d)
i'Kleisli = i'_ 

folding :: (Foldable t,Monoid w) => Iso' (a -> c) w -> (b -> a -> c) -> a -> t b -> c  
folding i f e t = yb i (foldMap (by i . f) t) e
foldlM :: (Foldable t,Monad m) => (a -> b -> m a) -> a -> t b -> m a
foldlM = folding (i'Kleisli.i'Endo) . flip
foldrM :: (Foldable t,Monad m) => (b -> a -> m a) -> t b -> a -> m a
foldrM = flip . folding (i'Kleisli.i'Endo.i'Dual)
findM :: (Foldable t,Monad m) => (a -> m (Maybe b)) -> t a -> m (Maybe b)
findM f = foldr fun (return Nothing)
  where fun a b = maybe b (return . Just) =<< f a

while :: Monad m => m Bool -> m ()
while e = fix (\w -> e >>= bool w unit)
until :: Monad m => m (Maybe a) -> m a
until e = fix (\w -> e >>= maybe w return)

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f a b = join (f<$>a<*>b)
(>>>=) :: Monad m => (m a,m b) -> (a -> b -> m c) -> m c
(a,b) >>>= f = bind2 f a b
bind3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bind3 f a b c = join (f<$>a<*>b<*>c)
(>>>>=) :: Monad m => (m a,m b,m c) -> (a -> b -> c -> m d) -> m d
(a,b,c) >>>>= f = bind3 f a b c

infixr 0 =<<
infixl 1 <*=,>>
(>>) :: Applicative f => f a -> f b -> f b
(>>) = (*>)
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \a -> g a >>= f
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = flip (<=<)
(<*=) :: Monad m => m a -> (a -> m b) -> m a
a <*= f = a >>= ((>>)<$>f<*>return)
only :: (Monoid (m ()),Monad m) => (a -> Bool) -> m a -> m a
only p m = m <*= guard . p
return :: Unit f => a -> f a
return = pure

joinMap :: Monad m => (a -> m b) -> m a -> m b
joinMap = (=<<)

coerceJoin :: forall m m' a. Monad m => (forall b. m b -> m' b) -> (m' (m' a) -> m' a)
coerceJoin _ = unsafeCoerce (join :: m (m a) -> m a)
coerceDuplicate :: forall m m' a. Comonad m => (forall b. m b -> m' b) -> (m' a -> m' (m' a))
coerceDuplicate _ = unsafeCoerce (duplicate :: m a -> m (m a))

newtype Compose' f g a = Compose' ((g:.:f) a)
                       deriving (Semigroup,Monoid,Unit,Functor,SemiApplicative,Applicative,MonadFix,Foldable)
i'Compose' :: Iso (Compose' f g a) (Compose' h i b) (g (f a)) (i (h b))
i'Compose' = i'Compose.iso Compose' (\(Compose' c) -> c)
instance Monad m => MonadTrans (Compose' m) where
  lift = by i'Compose' . map pure
instance Monad m => ConcreteMonad (Compose' m) where
  generalize = i'Compose' %%~ pure . yb i'Id
instance (Monad f,Monad g,Traversable f) => Monad (Compose' f g) where join = coerceJoin Compose'
instance (Traversable g,Traversable f) => Traversable (Compose' f g) where sequence = coerceSequence Compose'
deriving instance (Traversable f,Monad f,MonadState s g) => MonadState s (Compose' f g)
deriving instance (Traversable f,Monad f,MonadReader r g) => MonadReader r (Compose' f g)
deriving instance (Traversable f,Monad f,MonadWriter w g) => MonadWriter w (Compose' f g)



