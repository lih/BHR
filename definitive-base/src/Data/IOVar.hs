{-# LANGUAGE ExistentialQuantification #-}
module Data.IOVar (IOVar,newIOVar,readIOVar,writeIOVar) where

import Definitive
import Data.IORef

data IOVar w r = IOVar (IORef w) (w -> IO r)

undefRef :: IORef a
undefRef = newIORef undefined ^. thunk

instance Functor (IOVar w) where
  map f (IOVar r g) = IOVar r (map f . g)
instance Unit (IOVar w) where
  pure a = IOVar undefRef (const (pure a))
instance SemiApplicative (IOVar w)
instance Applicative (IOVar w)
instance Monad (IOVar w) where
  join (IOVar r f) = IOVar r (f >=> \(IOVar r' f') -> readIORef r' >>= f')

newIOVar :: a -> IO (IOVar a a)
newIOVar a = newIORef a <&> \r -> IOVar r pure
readIOVar :: IOVar w r -> IO r
readIOVar (IOVar ref k) = readIORef ref >>= k
writeIOVar :: IOVar w r -> w -> IO ()
writeIOVar (IOVar ref _) w = writeIORef ref w

