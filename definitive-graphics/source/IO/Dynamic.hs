{-# LANGUAGE CPP #-}
module IO.Dynamic (
  -- * The Dynamic type
  Callback,Dynamic,DynProps,callbacks,props,
  DynamicProperty(..),
  -- * Creating Dynamic values
  newDynamic,
  Trait(..),lensTrait,isoTrait,trait,
  -- * Altering Dynamic state
  runDynamicStateQual,setDynamicQual,
  runDynamicState,setDynamic,
  getDynamic,
  -- * Reacting to changes
  onChangeQualM,onChangeQual,onHighQual,onLowQual,
  onChangeM,onChange,onHigh,onLow,deleteCallbacks,
  -- * Convenience
  (<|)
  ) where

import Definitive
import Data.IORef

#include "definitive-graphics.h"

class Trait t where
  traitValue :: Lens' (t a) a
instance Trait DynProps where traitValue = props

class DynamicProperty w a p | p w -> a where
  property :: p -> Lens' w a

c'maybe :: Constraint (Maybe a)
c'maybe = c'_

data Callback props = forall a. Eq a => Callback (Maybe String) (FixFold' props a) (Maybe a -> Maybe a -> IO ())
                    | SelfCallback (Maybe String -> props -> IO ())
data DynProps props = DynProps { _propsCallbacks :: [Callback props]
                               , _propsValue :: props }
instance Eq props => Eq (DynProps props) where a==b = _propsValue a==_propsValue b
instance Ord props => Ord (DynProps props) where compare = comparing _propsValue
newtype Dynamic props = Dynamic { dynamicRef :: IORef (DynProps props,IO Bool,Bool) }
                      deriving Eq

callbacks :: Lens' (DynProps props) [Callback props]
callbacks = FIELD_LENS(_propsCallbacks)
props :: Lens' (DynProps props) props
props = FIELD_LENS(_propsValue)

newDynamic :: w -> IO (Dynamic w)
newDynamic w = Dynamic<$>newIORef (DynProps [] w,return False,False)

runDynamicStateQual :: Maybe String -> Dynamic props -> State (DynProps props) a -> IO a
runDynamicStateQual qual (Dynamic pr) st = do
  (a,isLate) <- runAtomic pr $ do
    ps <- getl l'1
    a <- l'1 |> st
    ps' <- getl l'1
    let canRun = case qual of
          Just s -> \x -> x/=Just s
          Nothing -> const True
        runCallbacks = do
          for_ (ps^.callbacks) $ \c -> case c of
            SelfCallback k -> k qual (ps'^.props)
            Callback q l k | canRun q -> let newVal = c'maybe (ps'^?props.l) ; oldVal = ps^?props.l
                                         in when (oldVal /= newVal) (k oldVal newVal)
                           | otherwise -> unit
    l'2 =~ \k -> fill True (k >> runCallbacks)
    isLate <- l'3 `swapWith` const True
    return (a,isLate)
  unless isLate $ do
    while $ join $ runAtomic pr $ l'2 `swapWith` const (return False)
    runAtomic pr (l'3 =- False)
  return a
setDynamicQual :: Maybe String -> Dynamic props -> FixFold' props a -> a -> IO ()
setDynamicQual q w p v = runDynamicStateQual q w (props.p =- v)
getDynamic :: Dynamic props -> Lens' props a -> IO a
getDynamic w l = by (l'1.props.l) <$> readIORef (dynamicRef w)
onChangeQualM :: Eq a => Maybe String -> FixFold' props a -> (Maybe a -> Maybe a -> IO ()) -> State (DynProps props) ()
onChangeQualM q l k = callbacks =~ (Callback q l k:)
onChangeQual :: Eq a => Maybe String -> Lens' props a -> (a -> a -> IO ()) -> State (DynProps props) ()
onChangeQual q l k = onChangeQualM q l (\a b -> sequence_ (liftA2 k a b))
onHighQual :: Eq a => Maybe String -> FixFold' props a -> (a -> IO ()) -> State (DynProps props) ()
onHighQual q l k = onChangeQualM q l (\_ x -> for_ x k)
onLowQual :: Eq a => Maybe String -> FixFold' props a -> IO () -> State (DynProps props) ()
onLowQual q l k = onChangeQualM q l (\_ x -> when (empty x) k)

setDynamic = setDynamicQual Nothing
runDynamicState = runDynamicStateQual Nothing
onChangeM :: Eq a => FixFold' props a -> (Maybe a -> Maybe a -> IO ()) -> State (DynProps props) ()
onChangeM = onChangeQualM Nothing
onChange :: Eq a => Lens' props a -> (a -> a -> IO ()) -> State (DynProps props) ()
onChange = onChangeQual Nothing
onHigh = onHighQual Nothing
onLow = onLowQual Nothing

deleteCallbacks :: Maybe String -> State (DynProps props) ()
deleteCallbacks Nothing = callbacks =- []
deleteCallbacks (Just x) = callbacks =~ \l -> [y | y <- l, case y of
                                                 Callback q _ _ -> q/=Just x
                                                 _ -> True]

infixl 3 <|
(<|) :: IO (Dynamic props) -> State (DynProps props) a -> IO (Dynamic props)
w <| st = w <*= \w' -> runDynamicState w' st
infixr 5 |>
(|>) :: Lens' a b -> State b x -> State a x
l |> st = l <~ yb state st

lensTrait :: (b -> a) -> Lens' a b -> IO (Dynamic b) -> IO (Dynamic a)
lensTrait mk l mw = mw >>= \(Dynamic ps) -> do
  let mk' (DynProps cs p,_,_) = (DynProps (SelfCallback (\_ x -> runAtomic ps (l'1.props =- x^.l)):map traitCallback cs) (mk p)
                                ,return False,False)
      traitCallback (Callback q l' k) = Callback q (l.l') k
      traitCallback (SelfCallback k) = SelfCallback (\q a -> k q (a^.l))
  ps' <- newIORef . mk' =<< readIORef ps
  let ret = Dynamic ps'
  runAtomic ps $ do l'1.callbacks =- [SelfCallback (\q n -> setDynamicQual q ret l n)]
  return ret
isoTrait :: Iso' a b -> IO (Dynamic b) -> IO (Dynamic a)
isoTrait i = lensTrait (yb i) i
trait :: Trait t => (a -> t a) -> IO (Dynamic a) -> IO (Dynamic (t a))
trait mk = lensTrait mk traitValue
