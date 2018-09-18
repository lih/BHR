{-# LANGUAGE UndecidableInstances #-}
module Algebra.Monad.State (
  -- * The State Monad
  MonadState(..),
  StateT,State,StateRes(..),
  stateT,state,
  (<~),(=~),(=-),(^>=),gets,getl,saving,swapWith,
  Next,Prev,
  mapAccum,mapAccum_,mapAccumR,mapAccumR_,scanl,scanr,push,pop,withPrev,withNext,

  -- * The State Arrow
  StateA(..),stateA,

  -- * Atomic IO state
  runAtomic
  ) where

import Algebra.Monad.RWS
import Algebra.Monad.Base
import Data.IORef

instance MonadState (IO ()) IO where
  get = return unit
  put a = a
  modify f = put (f unit)

newtype StateT s m a = StateT (RWST Void Void s m a)
                     deriving (Unit,Functor,SemiApplicative,Applicative,MonadFix,
                               MonadTrans,MonadInternal,
                               MonadCont,MonadState s,MonadList)
type State s a = StateT s Id a
instance Monad m => Monad (StateT s m) where join = coerceJoin StateT
instance MonadReader r m => MonadReader r (StateT s m) where
  ask = ask_ ; local = local_
instance MonadWriter w m => MonadWriter w (StateT s m) where
  tell = tell_ ; listen = listen_ ; censor = censor_
instance (MonadCounter w acc m) => MonadCounter w acc (StateT s m) where
  getCounter = getCounter_; setCounter = setCounter_
deriving instance MonadError e m => MonadError e (StateT s m)
deriving instance Semigroup (m (a,s,Void)) => Semigroup (StateT s m a)
deriving instance Monoid (m (a,s,Void)) => Monoid (StateT s m a)
deriving instance Semiring (m (a,s,Void)) => Semiring (StateT s m a)
deriving instance Ring (m (a,s,Void)) => Ring (StateT s m a)
deriving instance (Monad m,MonadFuture n m) => MonadFuture n (StateT s m)
deriving instance ConcreteMonad (StateT s)
instance (MonadLogic l m) => MonadLogic (StateT s l) (StateT s m) where
  deduce = coerceDeduce StateT StateT
  induce = coerceInduce StateT StateT

_StateT :: Iso (StateT s m a) (StateT t n b) (RWST Void Void s m a) (RWST Void Void t n b)
_StateT = iso StateT (\ ~(StateT s) -> s)
stateT :: (Functor m,Functor n) => Iso (StateT s m a) (StateT t n b) (s -> m (s,a)) (t -> n (t,b))
stateT = mapping (mapping $ iso (\ ~(s,a) -> (a,s,zero) ) (\(a,s,_) -> (s,a)))
          .promapping i'_.i'RWST._StateT
class StateRes t s a | t -> s a where
  evalS :: t -> a
  execS :: t -> s
instance StateRes (s,a) s a where evalS = snd ; execS = fst
instance (StateRes r a b) => StateRes (Id r) a b where evalS = evalS . getId ; execS = execS . getId
instance (StateRes r a b) => StateRes (s -> r) (s -> a) (s -> b) where evalS = map evalS ; execS = map execS
instance Functor m => StateRes (StateT s m a) (s -> m s) (s -> m a) where
  execS x = map2 execS (x^..stateT) ; evalS x = map2 evalS (x^..stateT)

state :: Iso (State s a) (State t b) (s -> (s,a)) (t -> (t,b))
state = mapping i'Id.stateT

(=-) :: MonadState s m => Fold' s s' -> s' -> m ()
infixl 1 =-,=~,<~
(<~) :: MonadState s m => Lens' s a -> (a -> (a,b)) -> m b
(<~) l st = getl l >>= \a -> let (a',b) = st a in b <$ modify (l %- a')
swapWith :: MonadState s m => Lens' s a -> (a -> a) -> m a
swapWith l f = l <~ \a' -> (f a',a')

l =- x = modify (set l x)
(=~) :: MonadState s m => Fold' s a -> (a -> a) -> m ()
l =~ f = modify (warp l f)
(^>=) :: MonadState s m => LensLike m a a s s -> (a -> m ()) -> m ()
l ^>= k = get >>= \s -> forl_ l s k
gets :: MonadState s m => (s -> a) -> m a
gets = (get<&>) 
getl :: MonadState s m => Lens' s a -> m a
getl l = gets (by l) 

saving :: MonadState s m => Lens' s s' -> m a -> m a
saving l st = getl l >>= \s -> st <* (l =- s)

-- * The State Arrow
newtype StateA m s a = StateA (StateT s m a)
stateA :: Iso (StateA m s a) (StateA m' s' a') (StateT s m a) (StateT s' m' a')
stateA = iso StateA (\(StateA s) -> s)

instance Monad m => Deductive (StateA m) where
  StateA sbc . StateA sab = StateA $ (^.stateT) $ \a ->
    (sab^..stateT) a >>= \(a',b) -> (a',).snd <$> (sbc^..stateT) b
instance Monad m => Category (StateA m) where
  id = StateA get
instance Monad m => Split (StateA m) where
  StateA sac <#> StateA sbd = StateA $ (^.stateT)
                              $ map2 (\((a',c),(b',d)) -> ((a',b'),(c,d)))
                              $ (Kleisli (sac^..stateT) <#> Kleisli (sbd^..stateT)) ^.. i'Kleisli
instance Monad m => Choice (StateA m) where
  StateA sac <|> StateA sbc = StateA $ (^.stateT) $
                              l Left (sac^..stateT)<|>l Right (sbc^..stateT)
    where l = map2 . first

mapAccum :: Traversable t => (a -> s -> (s, b)) -> t a -> s -> (s, t b)
mapAccum f t = traverse (by state<$>f) t^..state
mapAccum_ :: Traversable t => (a -> s -> (s, b)) -> t a -> s -> t b
mapAccum_ = (map.map.map) snd mapAccum
mapAccumR :: Traversable t => (a -> s -> (s, b)) -> t a -> s -> (s, t b)
mapAccumR f t = traverse (by (state.i'Backwards)<$>f) t^..state.i'Backwards
mapAccumR_ :: Traversable t => (a -> s -> (s, b)) -> t a -> s -> t b
mapAccumR_ = (map.map.map) snd mapAccumR
scanl :: Traversable t => (a -> b -> a) -> a -> t b -> t a
scanl f a tb = mapAccum_ (\b a' -> let x = f a' b in (x,x)) tb a
scanr :: Traversable t => (a -> b -> a) -> a -> t b -> t a
scanr f a tb = mapAccumR_ (\b a' -> let x = f a' b in (x,x)) tb a

push :: Traversable t => t a -> a -> t a
push = mapAccum_ (,)
pop :: Traversable t => t a -> a -> t a
pop = mapAccumR_ (,)

type Next a = a
type Prev a = a
withPrev :: Traversable t => a -> t a -> t (Prev a,a)
withPrev = flip (mapAccum_ (\a p -> (a,(p,a))))
withNext :: Traversable t => t a -> a -> t (a,Next a)
withNext = mapAccumR_ (\a p -> (a,(a,p)))

runAtomic :: IORef s -> State s a -> IO a
runAtomic r st = atomicModifyIORef r (yb state st)
