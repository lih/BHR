{-# LANGUAGE TypeFamilies, PatternSynonyms #-}
module Data.Matricial where

import Definitive

data Zero = Zero
data Succ x = Succ x

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three

_One = Succ Zero
_Two = Succ _One
_Three = Succ _Two
_Four = Succ _Three

data family Vec n :: * -> *
data instance Vec Zero a = V0
data instance Vec (Succ n) a = VS !a !(Vec n a)

instance (Eq a,Vector (Vec n)) => Eq (Vec n a) where a == b = and (liftA2 (==) a b)
instance (Ord a,Vector (Vec n)) => Ord (Vec n a) where compare a b = sum (liftA2 compare a b)

instance (Show a,Vector (Vec n)) => Show (Vec n a) where
  show v = "<"+intercalate ";" (map show v)+">"

instance Functor (Vec Zero) where
  map f V0 = V0
instance Functor (Vec n) => Functor (Vec (Succ n)) where
  map f (VS x xs) = VS (f x) (map f xs)
instance SemiApplicative (Vec Zero) where
  V0 <*> V0 = V0
instance SemiApplicative (Vec n) => SemiApplicative (Vec (Succ n)) where
  VS f fs <*> VS x xs = VS (f x) (fs<*>xs)
instance Unit (Vec Zero) where pure _ = V0
instance Unit (Vec n) => Unit (Vec (Succ n)) where pure x = VS x (pure x)
instance Applicative (Vec Zero)
instance Applicative (Vec n) => Applicative (Vec (Succ n))
instance Foldable (Vec Zero) where fold V0 = zero
instance Foldable (Vec n) => Foldable (Vec (Succ n)) where fold (VS a x) = a + fold x
instance Traversable (Vec Zero) where sequence V0 = pure V0
instance Traversable (Vec n) => Traversable (Vec (Succ n)) where sequence (VS a x) = VS<$>a<*>sequence x
class (Applicative v, Foldable v, Traversable v) => Vector v
instance Vector (Vec Zero)
instance Vector (Vec n) => Vector (Vec (Succ n))

type V1 = Vec One
type V2 = Vec Two
type V3 = Vec Three
type V4 = Vec Four

pattern V1 x = VS x V0
pattern V2 x y = VS x (V1 y)
pattern V3 x y z = VS x (V2 y z)
pattern V4 x y z t = VS x (V3 y z t)

instance (Semigroup a,Applicative (Vec n)) => Semigroup (Vec n a) where a + b = liftA2 (+) a b
instance (Monoid a,Applicative (Vec n)) => Monoid (Vec n a) where zero = pure zero
instance (Semiring a,Applicative (Vec n)) => Semiring (Vec n a) where a * b = liftA2 (*) a b
instance (Ring a,Applicative (Vec n)) => Ring (Vec n a) where one = pure one
instance (Disjonctive a, Applicative (Vec n)) => Disjonctive (Vec n a) where negate = map negate

type Mat n m a = Vec n (Vec m a)

scalar :: Iso a b (V1 a) (V1 b)
scalar = iso (\(V1 x) -> x) V1

matMult :: (Ring a, Vector (Vec n), Vector (Vec m), Vector (Vec p)) => Mat n m a -> Mat m p a -> Mat n p a
matMult x y = map (\vm -> map (\vm' -> scalProd vm vm') (transpose y)) x

scalProd :: (Ring a,Vector (Vec n)) => Vec n a -> Vec n a -> a
scalProd u v = sum (u*v)
scalProdM :: (Ring a,Vector (Vec n)) => Mat n n a -> Mat n n a -> Mat n n a
scalProdM u v = fromLinearMap (\w -> w * pure (scalProd (w & from scalar %~ ($* u)) (w & from scalar %~ ($* v))))

normalize :: (Vector (Vec n),Invertible a, RealFloat a) => Vec n a -> Vec n a
normalize v = map (/ sqrt (scalProd v v)) v

identityMat :: (Ring a, Vector (Vec n)) => Mat n n a
identityMat = mapAccum_ (\_ v -> (push v zero, v)) (pure ()) (push zero one)

fromLinearMap :: (Ring a, Vector (Vec n)) => (Vec n a -> Vec n a) -> Mat n n a
fromLinearMap f = map f identityMat

($*) = matMult

compScale x v = scalProd x v / scalProd x x
infixl 8 //
x // y = x - y * pure (compScale y x)
rotation :: (Eq a,Floating a,Invertible a,Vector (Vec n))
             => Vec n a -- ^ A source vector x
             -> Vec n a -- ^ A destination vector y
             -> Mat n n a -- ^ A linear transformation that maps x to y, otherwise preserving angles and relative scales
rotation x y = fromLinearMap (if x'==zero then (pure scale *) else rot)
  where x' = negate (y // x)
        y' = (x // y) * pure (scalProd y y / scalProd x x)
        scale = sqrt (scalProd y y / scalProd x x)
        rot v = pure scale * (v - (x * pure v_x + x' * pure v_x')) + y * pure v_x + y' * pure v_x'
          where v_x = compScale x v ; v_x' = compScale x' v

ejection :: (Invertible a,Vector (Vec n)) => Vec n a -> Mat n n a
ejection v = identityMat - (transpose (V1 v) $* pure (v * pure (recip (scalProd v v))))

skew :: (Ring a, Vector (Vec n)) => Vec n a -> Mat n n a
skew v = map (v*) identityMat

vector :: Ring a => Vector (Vec n) => Vec n a -> Vec (Succ n) a
vector v = pop (VS zero v) zero
point :: Ring a => Vector (Vec n) => Vec n a -> Vec (Succ n) a
point v = pop (VS zero v) one

v_x :: (Ring a,Vector (Vec n)) => Vec n a
v_x = push zero one
v_y :: (Ring a,Vector (Vec n)) => Vec n a
v_y = push v_x zero
v_z :: (Ring a,Vector (Vec n)) => Vec n a
v_z = push v_y zero
v_w :: (Ring a,Vector (Vec n)) => Vec n a
v_w = push v_z zero

translation :: (Ring a,Vector (Vec n)) => Vec n a -> Mat (Succ n) (Succ n) a
translation v = transpose (pop (push identityMat zero) (point v))


