module Algebra.Monad.Error (
  -- * The MonadError class
  MonadError(..),try,(!+),optional,throwIO,

  -- * The Either transformer
  EitherT,
  eitherT
  ) where

import Algebra.Monad.Base
import qualified Control.Exception as Ex

try :: MonadError e m => m a -> m a -> m a
try = catch . const
optional :: MonadError e m => m a -> m (Maybe a)
optional m = catch (\_ -> return Nothing) (Just<$>m)

(!+) :: MonadError Void m => m a -> m a -> m a
(!+) = flip try
infixr 0 !+

instance MonadError e (Either e) where
  throw = Left
  catch f = f<|>Right
instance MonadError Void [] where
  throw = const zero
  catch f [] = f zero
  catch _ l = l
newtype EitherT e m a = EitherT (Compose' (Either e) m a)
                      deriving (Unit,Functor,SemiApplicative,Applicative,MonadFix,Foldable,MonadTrans)
instance Monad m => Monad (EitherT e m) where join = coerceJoin EitherT
instance Traversable m => Traversable (EitherT e m) where sequence = coerceSequence EitherT
eitherT :: Iso (EitherT e m a) (EitherT f m b) (m (e:+:a)) (m (f:+:b))                              
eitherT = i'Compose'.iso EitherT (\(EitherT e) -> e)

instance MonadError Void Maybe where
  throw = const Nothing
  catch f Nothing = f zero
  catch _ a = a
instance MonadError Ex.SomeException IO where
  throw = Ex.throw
  catch = flip Ex.catch

throwIO :: (MonadError Ex.SomeException m,Ex.Exception e) => e -> m ()
throwIO = throw . Ex.toException

