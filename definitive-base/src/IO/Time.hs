{-# LANGUAGE TupleSections, RecursiveDo, Rank2Types, DeriveDataTypeable, ImplicitParams #-}
module IO.Time (
  -- * Unambiguous times
  Time,
  module Data.TimeVal,
  timeVal,freezed,protect,

  -- * Time utilities
  Seconds,
  timeIO,waitTill,currentTime,timeOrigin,

  -- * Conversion functions
  ms,mus,ns,minutes,hours,days
          
  ) where

import Definitive
import Control.Concurrent
import Data.TimeVal
import System.IO.Unsafe
import Data.IORef
import System.Clock
import Control.Exception (Exception(..))
import Data.Typeable

data Freezed = Freezed String
             deriving (Typeable,Show)
instance Exception Freezed  

freezed :: a
freezed = throw (toException (Freezed "val"))^.thunk

protect :: TimeVal t -> TimeVal t
protect = thunk %%~ try (pure Never)

-- |A type wrappers for timestamps that can be compared unambiguously
data Time t = Time (TimeVal t -> TimeVal t) (TimeVal t -> TimeVal t)
instance Show t => Show (Time t) where show = show . timeVal
instance Ord t => Eq (Time t) where
  a == b = compare a b == EQ
instance Ord t => Ord (Time t) where
  compare ~(Time fa fa') ~(Time fb fb') =
    unamb (cmp fa fb') (invertOrd (cmp fb fa'))
    where cmp f f' = compare a (protect (f' a))
            where a = protect (f maxBound)
-- |The Time semigroup where @ta + tb == max ta tb@
instance Ord t => Semigroup (Time t) where
  ~(Time fa fb) + ~(Time fa' fb') = Time (mapTL mini fa fa') (mapTL maxi fb fb')
    where mini h x x' = if h < x then x else max x x'
          maxi h x x' = if h > x then max x x' else x
-- |The Time monoid where @zero == minBound@
instance Ord t => Monoid (Time t) where
  zero = minBound
-- |The Time ring where @(*) == min@ and @one == maxBound@
instance Ord t => Semiring (Time t) where
  ~(Time fa fb) * ~(Time fa' fb') = Time (mapTL mini fa fa') (mapTL maxi fb fb')
    where mini h x x' = if h < x then min x x' else x
          maxi h x x' = if h > x then x else min x x'
instance Ord t => Ring (Time t) where
  one = maxBound

type TimeFun t = TimeVal t -> TimeVal t
mapTL :: (TimeVal t -> TimeVal t -> TimeFun t) -> TimeFun t -> TimeFun t -> TimeFun t
mapTL _max fa fa' h = _max h x x'`unamb`_max h x' x
  where x = protect (fa h) ; x' = protect (fa' h)

instance Bounded (Time t) where
  minBound = Time (pure minBound) (pure minBound)
  maxBound = Time (pure maxBound) (pure maxBound)
instance Unit Time where
  pure t = Time (pure (pure t)) (pure (pure t)) 

type Seconds = Double

-- |A Time's pure value. Reduction to normal form may block.
timeVal :: Time t -> TimeVal t
timeVal (Time fa _) = protect (fa maxBound)

-- |Constructs a Time/value pair representing the delayed computation of the argument.
timeIO :: IO a -> IO (Time Seconds,a)
timeIO io = do
  sem <- newEmptyMVar
  ret <- newIORef id
  res <- newEmptyMVar
  
  minAction <- newIORef $ \tm -> readIORef ret <**> amb (readMVar sem) (
    Since<$>case tm of
      Always -> currentTime
      Since t -> waitTill t >> currentTime
      Never -> throw (toException $ Freezed "min"))
  maxAction <- newIORef $ \tm -> readIORef ret <**> amb (readMVar sem) (
    case tm of
      Always -> throw (toException (Freezed "max"))
      Since t -> waitTill t >> pure Never
      Never -> Since<$>currentTime)
    
  let refAction ref = \t -> unsafePerformIO (join (readIORef ref<*>pure t))
  _ <- forkIO $ void $ mfix $ \t -> do 
    t' <- catch (\_ -> return Never) (io >>= putMVar res >> return (pure t))
    writeIORef minAction (const (pure t'))
    writeIORef maxAction (const (pure t'))
    writeIORef ret (const t')
    putMVar sem t'
    currentTime
    
  return (Time (refAction minAction) (refAction maxAction),unsafePerformIO (readMVar res))
  
waitTill :: Seconds -> IO ()
waitTill t = do
  now <- t `seq` currentTime
  when (t>now) $ threadDelay (floor $ (t-now)*1000000)

seconds :: TimeSpec -> Seconds
seconds t = fromIntegral (sec t) + fromIntegral (nsec t)/1000000000 :: Seconds
currentTime :: IO Seconds
currentTime = seconds<$>getTime Realtime
timeOrigin :: (( ?birthTime :: Seconds ) => IO a) -> IO a
timeOrigin m = currentTime >>= \t -> let ?birthTime = t in m

ms,mus,ns,minutes,hours,days :: Seconds -> Seconds
ms = (/1000)
mus = (/1000000)
ns = (/1000000000)
minutes = (*60)
hours = (*3600)
days = (*(3600*24))
