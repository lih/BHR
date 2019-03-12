module Graphics.Widget.GL.Vertex where

import Definitive

-- |A two-element vector
data V2 t = V2 !t !t
          deriving Show
instance Semigroup a => Semigroup (V2 a) where (+) = plusA
instance Monoid a => Monoid (V2 a) where zero = zeroA
instance Functor V2 where map f (V2 x y) = V2 (f x) (f y)
instance Unit V2 where pure = join V2
instance SemiApplicative V2 where
  V2 fx fy <*> V2 x y = V2 (fx x) (fy y)
instance Applicative V2 where
instance Foldable V2 where
  fold (V2 a b) = a+b
instance Traversable V2 where
  sequence (V2 a b) = V2<$>a<*>b
instance Ord t => Eq (V2 t) where
  a == b = compare a b == EQ
instance Ord t => Ord (V2 t) where
  compare (V2 x y) (V2 x' y')
    | x<x' && y<y' = LT
    | x>x' && y>y' = GT
    | otherwise = EQ
instance Lens1 a a (V2 a) (V2 a) where
  l'1 = lens (\(V2 a _) -> a) (\ (V2 _ c) b -> V2 b c)
instance Lens2 a a (V2 a) (V2 a) where
  l'2 = lens (\(V2 _ a) -> a) (\ (V2 c _) b -> V2 c b)

-- |A three-element vector
data V3 t = V3 !t !t !t
          deriving Show
instance Semigroup a => Semigroup (V3 a) where (+) = plusA
instance Monoid a => Monoid (V3 a) where zero = zeroA
instance Functor V3 where map f (V3 x y z) = V3 (f x) (f y) (f z)
instance Unit V3 where pure = (join.join) V3
instance SemiApplicative V3 where
  V3 fx fy fz <*> V3 x y z = V3 (fx x) (fy y) (fz z)
instance Applicative V3 where
instance Foldable V3 where
  fold (V3 a b c) = a+b+c
instance Traversable V3 where
  sequence (V3 a b c) = V3<$>a<*>b<*>c
instance Ord t => Eq (V3 t) where
  a == b = compare a b == EQ
instance Ord t => Ord (V3 t) where
  compare (V3 x y z) (V3 x' y' z')
    | x<x' && y<y' && z<z' = LT
    | x>x' && y>y' && z>z' = GT
    | otherwise = EQ
instance Lens1 x x (V3 x) (V3 x) where
  l'1 = lens (\(V3 x _ _) -> x) (\(V3 _ y z) x -> V3 x y z)
instance Lens2 y y (V3 y) (V3 y) where
  l'2 = lens (\(V3 _ y _) -> y) (\(V3 x _ z) y -> V3 x y z)
instance Lens3 z z (V3 z) (V3 z) where
  l'3 = lens (\(V3 _ _ z) -> z) (\(V3 x y _) z -> V3 x y z)

-- |A three-element vector
data V4 t = V4 !t !t !t !t
          deriving Show
instance Semigroup a => Semigroup (V4 a) where (+) = plusA
instance Monoid a => Monoid (V4 a) where zero = zeroA
instance Functor V4 where map f (V4 x y z w) = V4 (f x) (f y) (f z) (f w)
instance Unit V4 where pure = (join.join.join) V4
instance SemiApplicative V4 where
  V4 fx fy fz fw <*> V4 x y z w = V4 (fx x) (fy y) (fz z) (fw w)
instance Applicative V4 where
instance Foldable V4 where
  fold (V4 a b c d) = a+b+c+d
instance Traversable V4 where
  sequence (V4 a b c d) = V4<$>a<*>b<*>c<*>d
instance Ord t => Eq (V4 t) where
  a == b = compare a b == EQ
instance Ord t => Ord (V4 t) where
  compare (V4 x y z w) (V4 x' y' z' w')
    | x<x' && y<y' && z<z' && w<w' = LT
    | x>x' && y>y' && z>z' && w>w'= GT
    | otherwise = EQ
instance Lens1 x x (V4 x) (V4 x) where
  l'1 = lens (\(V4 x _ _ _) -> x) (\(V4 _ y z t) x -> V4 x y z t)
instance Lens2 y y (V4 y) (V4 y) where
  l'2 = lens (\(V4 _ y _ _) -> y) (\(V4 x _ z t) y -> V4 x y z t)
instance Lens3 z z (V4 z) (V4 z) where
  l'3 = lens (\(V4 _ _ z _) -> z) (\(V4 x y _ t) z -> V4 x y z t)
instance Lens4 t t (V4 t) (V4 t) where
  l'4 = lens (\(V4 _ _ _ t) -> t) (\(V4 x y z _) t -> V4 x y z t)
    
i'V4 :: Iso (t,t,t,t) (u,u,u,u) (V4 t) (V4 u)
i'V4 = iso (\(V4 x y z t) -> (x,y,t,z)) (\(x,y,z,t) -> V4 x y z t)
