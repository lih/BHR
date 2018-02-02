{-# LANGUAGE UndecidableInstances, ScopedTypeVariables #-}
module Algebra.Monad.Logic where

import Algebra.Monad.Base
import Algebra.Monad.Writer

newtype LogicT m a = LogicT { runLogicT :: forall r. (a -> m r -> m r) -> m r -> m r }

instance Functor (LogicT m) where
  map f (LogicT l) = LogicT (\k -> l (\a -> k (f a)))
instance Unit (LogicT m) where
  pure a = LogicT ($a)
instance SemiApplicative (LogicT m)
instance Applicative (LogicT m)
instance Monad (LogicT m) where
  join (LogicT l) = LogicT (\k -> l (\(LogicT l') -> l' k))
instance MonadFix m => MonadFix (LogicT m) where
  mfix f = map pure (mfix (map head . (^..listLogic) . f))^.listLogic
instance MonadTrans LogicT where
  lift ma = LogicT (\k mr -> ma >>= \a -> k a mr)
instance (Monad m,Foldable m) => Foldable (LogicT m) where
  fold (LogicT l) = fold $ l (\a m -> map (a+) m) (pure zero) 
instance (Monad m,Traversable m) => Traversable (LogicT m) where
  sequence l = traverse sequence (l^..listLogic) <&> (^.listLogic)

instance Semigroup (LogicT m a) where
  LogicT l + LogicT l' = LogicT (\k -> l k . l' k)
instance Monoid (LogicT m a) where
  zero = LogicT (pure id)
instance Semigroup a => Semiring (LogicT m a) where
  (*) = plusA
instance Monoid a => Ring (LogicT m a) where
  one = zeroA

instance MonadState s m => MonadState s (LogicT m) where
  get = lift get
  modify f = lift (modify f)
instance MonadReader r m => MonadReader r (LogicT m) where
  ask = lift ask
  local f (LogicT l) = LogicT (\k mr -> local f (l k mr))
instance MonadWriter w m => MonadWriter w (LogicT m) where
  tell = lift . tell
  listen l = induce (listen (deduce l) <&> \(w,ml) -> map ((w,) <#> listen) ml)
  censor l = induce (censor (deduce l <&> \ml -> case ml of
                                Just ((a,f),l') -> (Just (a,censor l'),f)
                                Nothing -> (Nothing,id)))
instance Monad m => MonadError Void (LogicT m) where
  throw _ = zero
  catch z (LogicT l) = LogicT l'
    where l' k mr = l k' (Left<$>mr) >>= \x -> case x of
            Left r -> runLogicT (z zero) k (pure r)
            Right r -> return r
            where k' a m = Right <$> k a (map (id<|>id) m)

instance Monad m => MonadLogic m (LogicT m) where
  deduce l = runLogicT l (\a m -> pure (pure (a,induce m))) (pure zero)
  induce mm = LogicT (\k m -> mm >>= maybe m (\(a,l) -> k a (runLogicT l k m)))

listLogic :: (MonadLogic m l,MonadLogic n l') => Iso (l a) (l' b) (m [a]) (n [b])
listLogic = iso alts deduceAll
  where alts m = induce (m <&> \l -> case l of
          [] -> Nothing
          (a:t) -> Just (a,alts (pure t)))
deduction :: (MonadLogic m l,MonadLogic m' l') => Iso (m (Maybe (a,l a))) (m' (Maybe (b,l' b))) (l a) (l' b) 
deduction = iso deduce induce

deduceMany :: MonadLogic m l => Int -> l a -> m [a]
deduceMany 0 _ = pure []
deduceMany n l = deduce l >>= maybe (pure []) (\(a,t) -> (a:)<$>deduceMany (n-1) t)
deduceAll :: MonadLogic m l => l a -> m [a]
deduceAll l = deduce l >>= maybe (pure []) (\(a,t) -> (a:)<$>deduceAll t)

logicChoose :: MonadLogic m l => [a] -> l a
logicChoose l = pure l^.listLogic

cut :: MonadLogic m l => l a -> l a
cut = deduction %~ map (traverse.l'2.deduction %- pure Nothing)


