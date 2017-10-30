module Main where

import Definitive
import Language.Parser

data TimeStamp = TimeStamp Double
instance Semigroup TimeStamp where
  TimeStamp a + TimeStamp b = TimeStamp (max a b)
instance Monoid TimeStamp where
  zero = TimeStamp 0

newtype GrowT m a = GrowT { runGrowT :: RWST (GrowEnv m) TimeStamp () m a }
                  deriving (Functor, Unit, SemiApplicative, Applicative, MonadFix, MonadReader (GrowEnv m), MonadWriter TimeStamp)
instance Monad m => Monad (GrowT m) where
  join = coerceJoin GrowT

data GrowNode a = GrowMap (Map String a)
                | GrowFunc String a
                | GrowApply a a
                | GrowDelayed a
data Builtin m = B_In_0 | B_In_1 (GrowThunk m)
data GrowLeaf m = GrowString String | GrowBuiltin (Builtin m)
type GrowThunk m = Free (GrowNode :.: GrowT m) (GrowLeaf m)
type GrowEnv m = Map String (GrowThunk m)

rnf :: Monad m => GrowThunk m -> GrowT m (GrowThunk m)
rnf (Pure l) = pure (Pure l)
rnf th@(Join (Compose n)) = case n of
  GrowApply mf mx -> mf >>= \f -> do
    case f of
      Pure (GrowBuiltin B_In_0) -> pure $ Pure $ GrowBuiltin $ B_In_1 (Join . Compose $ GrowDelayed mx)
      Pure s -> error "Cannot apply a string"
      Join (Compose (GrowFunc s a)) -> local (insert s (Join . Compose $ GrowDelayed mx)) (rnf =<< a)
      Join (Compose (GrowDelayed x)) -> x
  _ -> pure th
   

main = do
  putStrLn "Hello, world !"
