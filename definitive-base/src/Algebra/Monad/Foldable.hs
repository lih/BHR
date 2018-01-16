{-# LANGUAGE UndecidableInstances #-}
module Algebra.Monad.Foldable (
  -- * The MonadList class
  MonadList(..),
  
  -- * Foldable monads transformers
  -- ** The List transformer
  ListT,listT,
  -- ** The Tree transformer
  TreeT(..),treeT,
  -- ** The Maybe transformer
  MaybeT(..),maybeT,
  -- ** The Strict Monad transformer
  StrictT(..),strictT
  ) where

import Algebra.Monad.Base
import Algebra.Monad.RWS
import Data.Tree (Tree(..))

instance MonadList [] where choose = id

newtype ListT m a = ListT (Compose' [] m a)
                    deriving (Semigroup,Monoid,
                              Functor,SemiApplicative,Applicative,Unit,Foldable,MonadTrans)
instance Monad m => Monad (ListT m) where join = coerceJoin ListT
instance Traversable m => Traversable (ListT m) where sequence = coerceSequence ListT
listT :: Iso (ListT m a) (ListT m' a') (m [a]) (m' [a'])
listT = i'Compose'.iso ListT (\(ListT l) -> l)
instance Monad m => MonadList (ListT m) where
  choose = by listT . return 
instance MonadFix m => MonadFix (ListT m) where
  mfix f = by listT (mfix (yb listT . f . head))
instance MonadState s m => MonadState s (ListT m) where
  get = get_ ; modify = modify_ ; put = put_
instance MonadWriter w m => MonadWriter w (ListT m) where
  tell = lift.tell
  listen = listT-.map sequence.listen.-listT
  censor = listT-.censor.map (\l -> (fst<$>l,compose (snd<$>l))).-listT
instance Monad m => MonadError Void (ListT m) where
  throw = const zero
  catch f mm = mm & listT %%~ (\m -> m >>= \_l -> case lazy (traverse Strict _l) of
                                   [] -> f zero^..listT
                                   [x] -> pure [x]
                                   l -> pure l)

newtype TreeT m a = TreeT (Compose' Tree m a)
                  deriving (Functor,Unit,SemiApplicative,Applicative,MonadFix,Foldable,MonadTrans)
instance Monad m => Monad (TreeT m) where join = coerceJoin TreeT
instance Traversable m => Traversable (TreeT m) where sequence = coerceSequence TreeT
treeT :: Iso (TreeT m a) (TreeT n b) (m (Tree a)) (n (Tree b))
treeT = i'Compose'.iso TreeT (\(TreeT t) -> t)

newtype MaybeT m a = MaybeT (Compose' Maybe m a)
                  deriving (Functor,SemiApplicative,Unit,Applicative,MonadFix,Foldable,MonadTrans)
instance Monad m => Monad (MaybeT m) where join = coerceJoin MaybeT
instance Traversable m => Traversable (MaybeT m) where sequence = coerceSequence MaybeT
maybeT :: Iso (MaybeT m a) (MaybeT m' b) (m (Maybe a)) (m' (Maybe b))
maybeT = i'Compose'.iso MaybeT (\(MaybeT m) -> m)

newtype StrictT m a = StrictT (Compose' Strict m a)
                    deriving (Functor,SemiApplicative,Unit,Applicative,MonadFix,Foldable,MonadTrans)
instance Monad m => Monad (StrictT m) where join = coerceJoin StrictT
instance Traversable m => Traversable (StrictT m) where sequence = coerceSequence StrictT
strictT :: Iso (StrictT m a) (StrictT m' b) (m (Strict a)) (m' (Strict b))
strictT = i'Compose'.iso StrictT (\(StrictT s) -> s)
