{-# LANGUAGE UndecidableInstances #-}
module Algebra.Monad.RWS (
  RWST(..),RWS,MonadInternal(..),i'RWST,

  -- * Default methods
  get_,put_,modify_,local_,ask_,tell_,listen_,censor_,getCounter_,setCounter_
  ) where

import Algebra.Monad.Base

newtype RWST r w s m a = RWST { runRWST :: (r,s) -> m (a,s,w) }
type RWS r w s a = RWST r w s Id a

-- Instances
instance (Unit f,Monoid w) => Unit (RWST r w s f) where
  pure a = RWST (\ ~(_,s) -> pure (a,s,zero))
instance Functor f => Functor (RWST r w s f) where
  map f (RWST fa) = RWST (fa >>> map (\ ~(a,s,w) -> (f a,s,w)))
instance (Monoid w,Monad m) => SemiApplicative (RWST r w s m)
instance (Monoid w,Monad m) => Applicative (RWST r w s m)
instance (Monoid w,Monad m) => Monad (RWST r w s m) where
  join mm = RWST (\ ~(r,s) -> do
                     ~(m,s',w) <- runRWST mm (r,s)
                     ~(a,s'',w') <- runRWST m (r,s')
                     return (a,s'',w+w'))
instance (Monoid w,MonadFix m) => MonadFix (RWST r w s m) where
  mfix f = RWST (\x -> mfix (\ ~(a,_,_) -> runRWST (f a) x))
instance (Monoid w,MonadCont m) => MonadCont (RWST r w s m) where
  callCC f = RWST $ \i ->
    callCC (\k -> runRWST (f (\a -> RWST (\(_,s) -> k (a,s,zero)<&>(,s,zero)))) i<&> \(b,_,_) -> b)
deriving instance Semigroup (m (a,s,w)) => Semigroup (RWST r w s m a)
deriving instance Monoid (m (a,s,w)) => Monoid (RWST r w s m a)
deriving instance Semiring (m (a,s,w)) => Semiring (RWST r w s m a)
deriving instance Ring (m (a,s,w)) => Ring (RWST r w s m a)
instance (Monad m,Monoid w) => MonadState s (RWST r w s m) where
  get = RWST (\ ~(_,s) -> pure (s,s,zero) )
  put s = RWST (\ _ -> pure ((),s,zero) )
  modify f = RWST (\ ~(_,s) -> pure ((),f s,zero) )
instance (Monad m,Monoid w) => MonadReader r (RWST r w s m) where
  ask = RWST (\ ~(r,s) -> pure (r,s,zero) )
  local f (RWST m) = RWST (\ ~(r,s) -> m (f r,s) )
instance (Monad m,Monoid w) => MonadWriter w (RWST r w s m) where
  tell w = RWST (\ ~(_,s) -> pure ((),s,w) )
  listen (RWST m) = RWST (m >>> map (\ ~(a,s,w) -> ((w,a),s,w) ) )
  censor (RWST m) = RWST (m >>> map (\ ~(~(a,f),s,w) -> (a,s,f w) ) )

instance Foldable m => Foldable (RWST Void w Void m) where
  fold (RWST m) = foldMap (\(w,_,_) -> w).m $ (zero,zero)
instance Traversable m => Traversable (RWST Void w Void m) where
  sequence (RWST m) = map (RWST . const . map (\((s,w),a) -> (a,s,w)))
                      . sequence . map (\(a,s,w) -> sequence ((s,w),a))
                      $ m (zero,zero)

instance (Monoid w,MonadError e m) => MonadError e (RWST r w s m) where
  throw = lift.throw
  catch f (RWST m) = RWST (\x -> catch (flip runRWST x.f) (m x))
instance (Monoid w,MonadList m) => MonadList (RWST r w s m) where
  fork = lift . fork
instance (Monoid w,MonadLogic l m) => MonadLogic (RWST r w s l) (RWST r w s m) where
  deduce (RWST k) = RWST $ \x@(_,s) -> deduce (k x) <&> \y -> case y of
    Just ((a,_,w),l) -> (Just (a,RWST (const l)),s,w)
    Nothing -> (Nothing,s,zero)
  induce (RWST k) = RWST $ \x -> induce $ k x <&> \y -> case y of
    (Just (a,RWST l),s,w) -> Just ((a,s,w),l x)
    (Nothing,_,_) -> Nothing

instance Monoid w => MonadTrans (RWST r w s) where
  lift m = RWST (\ ~(_,s) -> (,s,zero) <$> m)
instance Monoid w => ConcreteMonad (RWST r w s) where
  generalize (RWST s) = RWST (\x -> pure (s x^..i'Id))
instance (Monoid w) => MonadInternal (RWST r w s) where
  internal f (RWST m) = RWST (\ x -> f (m x <&> \ ~(a,s,w) -> ((s,w),a) )
                                     <&> \ ~((s,w),b) -> (b,s,w) )

instance (Monad m, Monoid w, MonadFuture n m) => MonadFuture n (RWST r w s m) where
  future = lift . future

class MonadTrans t => MonadInternal t where
  internal :: Monad m => (forall c. m (c,a) -> m (c,b)) ->
              (t m a -> t m b)

i'RWST :: Iso (RWST r w s m a) (RWST r' w' s' m' a')
         ((r,s) -> m (a,s,w)) ((r',s') -> m' (a',s',w'))
i'RWST = iso RWST runRWST

get_ :: (MonadTrans t, MonadState a m) => t m a
get_ = lift get
put_ :: (MonadTrans t, MonadState s m) => s -> t m ()
put_ = lift . put
modify_ :: (MonadTrans t, MonadState s m) => (s -> s) -> t m ()
modify_ = lift . modify  
ask_ :: (MonadTrans t, MonadReader a m) => t m a
ask_ = lift ask
local_ :: (MonadInternal t, MonadReader r m) => (r -> r) -> t m a -> t m a
local_ f = internal (local f)
tell_ :: (MonadWriter w m, MonadTrans t) => w -> t m ()
tell_ = lift . tell
listen_ :: (MonadInternal t, MonadWriter w m) => t m a -> t m (w, a)
listen_ = internal (\m -> listen m <&> \(w,(c,a)) -> (c,(w,a)) )
censor_ :: (MonadInternal t, MonadWriter w m) => t m (a, w -> w) -> t m a
censor_ = internal (\m -> censor (m <&> \(c,(a,f)) -> ((c,a),f)))
getCounter_ :: (MonadTrans t,MonadCounter w acc m) => t m acc
getCounter_ = lift getCounter
setCounter_ :: (MonadTrans t,MonadCounter w acc m) => acc -> t m ()
setCounter_ c = lift (setCounter c)
