{-# LANGUAGE Rank2Types, MultiParamTypeClasses, FunctionalDependencies, ViewPatterns, TupleSections, LiberalTypeSynonyms #-}
{-|
A module providing simple Lens functionality.

Lenses are a Haskell abstraction that allows you to access and modify
part of a structure, compensating for and improving upon Haskell's
horrendous record syntax and giving Haskell a first-class record system.

This module defines four kinds of Lenses : Folds that allow multiple
read/writes into a structure; Traversals that allow parallel read/writes;
Lenses that allow just a single read/write; and Isos which allow a read and
a write in both directions. Lenses of any kind can be composed with
@(.)@, yielding a Lens of the most general kind, so that composing a
Lens with an Iso yields a Lens, and a Traversal with an Iso or a Lens
yields a Traversal.

-} 
module Algebra.Lens(
  -- * The lens types
  LensLike,
  FixFold,FixFold',
  Fold,Fold',
  Traversal,Traversal',
  Lens,Lens',
  Iso,Iso',(:<->:),
  
  -- * Constructing lenses
  iso,from,lens,getter,prism,sat,simple,(.+),forl,forl_,traversel,traversel_,

  -- * Extracting values
  (^.),($^),(^..),(^?),has,(^??),(%~),(%-),(%%~),(%%-),by,yb,warp,set,
  (-.),(.-),
  
  -- * Basic lenses
  Lens1(..),Lens2(..),Lens3(..),Lens4(..),Lens5(..),Lens6(..),Lens7(..),Lens8(..),Lens9(..),
  Trav1(..),Trav2(..),Trav3(..),Trav4(..),Trav5(..),Trav6(..),Trav7(..),Trav8(..),Trav9(..),
  Compound(..),
  i'list,i'pair,t'head,t'tail,t'Just,l'Just,
  
  -- * Isomorphisms
  Isomorphic(..),

  -- ** Miscellaneous
  thunk,chunk,curried,

  -- ** Type wrappers
  i'Id,i'OrdList,i'Const,i'Dual,i'Endo,i'Flip,i'maybe,i'Max,i'Compose,i'Backwards,i'Accum,

  -- ** Algebraic isomorphisms
  negated,commuted,adding,
  
  -- ** Higher-order isomorphisms
  warp2,mapping,mapping',promapping,applying,

  IsoFunctor(..),(<.>),IsoFunctor2(..)
  ) where

import Algebra.Core hiding (flip)
import Algebra.Functor
import Algebra.Applicative
import Algebra.Classes
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (evaluate)
import Data.ByteString.Lazy (toStrict,fromStrict)

type LensLike f s t a b = (s -> f t) -> (a -> f b)
type Simple f a b = f b b a a

type FixFold s t a b = forall m. MonadFix m => LensLike m s t a b
type FixFold' a b = Simple FixFold a b
type Fold s t a b = forall m. Monad m => LensLike m s t a b
type Fold' a b = Simple Fold a b
type Traversal s t a b = forall f. Applicative f => LensLike f s t a b
type Traversal' a b = Simple Traversal a b
type Lens s t a b = forall f.Functor f => LensLike f s t a b
type Lens' a b = Simple Lens a b
type Iso s t a b = forall p f. (Functor f,Bifunctor p) => p s (f t) -> p a (f b)
type Iso' a b = Simple Iso a b
type a :<->: b = Iso' a b

data IsoT a b s t = IsoT (s -> a) (b -> t)
instance Functor (IsoT a b s) where map f (IsoT u v) = IsoT u (map f v)
instance Cofunctor (Flip (IsoT a b) t) where
  comap f (Flip (IsoT u v)) = Flip (IsoT (promap f u) v)
instance Bifunctor (IsoT a b)

-- |Create an 'Iso' from two inverse functions.
iso :: (a -> s) -> (t -> b) -> Iso s t a b
iso f g = dimap f (map g)
isoT :: Iso s t a b -> IsoT s t a b
isoT i = getId<$>i (IsoT id Id)
unIsoT :: IsoT s t a b -> Iso s t a b
unIsoT (IsoT u v) = iso u v
-- |Reverse an 'Iso'
--
-- @
-- from :: 'Iso'' a b -> 'Iso'' b a
-- @
from :: Iso s t a b -> Iso b a t s
from i = let ~(IsoT u v) = isoT i in unIsoT (IsoT v u)
-- |Create a 'Lens' from a getter and setter function.
-- 
-- @
-- lens :: (a -> b) -> (a -> b -> a) -> 'Lens'' a b
-- @
lens :: (a -> s) -> (a -> t -> b) -> Lens s t a b
lens f g = \k a -> g a <$> k (f a) 

getter :: (a -> b) -> Lens' a b
getter f = \k a -> a<$k (f a)

-- |Create a 'Traversal' from a maybe getter and setter function.
--
-- @
-- prism :: (a -> (a:+:b)) -> (a -> b -> a) -> 'Traversal'' a b
-- @
prism :: (a -> (b:+:s)) -> (a -> t -> b) -> Traversal s t a b 
prism f g = \k a -> (pure <|> map (g a) . k) (f a)

simple :: LensLike f a b a b -> LensLike f a b a b
simple l = l

sat :: (a -> Bool) -> Traversal' a a
sat p = \k a -> (if p a then k else pure) a

(.+) :: Monad m => LensLike m s t a b -> LensLike m s t b c -> LensLike m s t a c
f .+ f' = \k a -> f k a >>= f' k
infixr 8 .+

-- |Retrieve a value from a structure using a 'Lens' (or 'Iso')
infixl 8 ^.,^..,^?,^??,%~,%-,%%~,%%-
infixr 8 `yb`
infixr 0 $^
(^.) :: a -> Lens b b a a -> b
x^.l = by l x
(^..) :: a -> Iso a a b b -> b
x^..i = yb i x
($^) :: Lens b b a a -> a -> b
($^) = by

-- |
(%~) :: FixFold s t a b -> (s -> t) -> (a -> b)
(%~) = warp
(%%~) :: Iso s t a b -> (b -> a) -> (t -> s)
(%%~) i = warp (from i)
(%-) :: FixFold s t a b -> t -> (a -> b)
(%-) = set
(%%-) :: Iso s t a b -> a -> (t -> s)
(%%-) i = set (from i)
(^?) :: (Unit f,Monoid (f b),MonadFix ((,) (f b))) => a -> FixFold' a b -> f b
x^?l = fst $ l (\y -> (pure y,y)) x
(^??) :: MonadFix ((,) [b]) => a -> FixFold' a b -> [b]
x^??l = fst $ l (\y -> ([y],y)) x

(-.) :: Lens c u b v -> (a -> b) -> a -> c
l-.f = by l.f
(.-) :: (b -> c) -> Iso a a b b -> a -> c
f.-i = f.yb i
infixr 9 -.,.-
by :: Lens b u a v -> a -> b
by l = getConst . l Const
yb :: Iso s t a b -> t -> b
yb i = by (from i)
warp :: FixFold s t a b -> (s -> t) -> (a -> b)
warp l = map getId . l . map Id
set :: FixFold s t a b -> t -> (a -> b)
set l = warp l . const 

forl :: LensLike f a b c d -> c -> (a -> f b) -> f d
forl l c f = l f c
forl_ :: Functor f => LensLike f a a c c -> c -> (a -> f ()) -> f ()
forl_ l c f = void $ l (\a -> a<$f a) c
traversel :: LensLike f a b c d -> (a -> f b) -> c -> f d
traversel l = flip (forl l)
traversel_ :: Functor f => LensLike f a a c c -> (a -> f ()) -> c -> f ()
traversel_ l = flip (forl_ l)

class Lens1 s t a b | a -> s, a t -> b where
  l'1 :: Lens s t a b
class Lens2 s t a b | a -> s, a t -> b where
  l'2 :: Lens s t a b
class Lens3 s t a b | a -> s, a t -> b where
  l'3 :: Lens s t a b
class Lens4 s t a b | a -> s, a t -> b where
  l'4 :: Lens s t a b
class Lens5 s t a b | a -> s, a t -> b where
  l'5 :: Lens s t a b
class Lens6 s t a b | a -> s, a t -> b where
  l'6 :: Lens s t a b
class Lens7 s t a b | a -> s, a t -> b where
  l'7 :: Lens s t a b
class Lens8 s t a b | a -> s, a t -> b where
  l'8 :: Lens s t a b
class Lens9 s t a b | a -> s, a t -> b where
  l'9 :: Lens s t a b
class Trav1 s t a b | a -> s, a t -> b where
  t'1 :: Traversal s t a b
class Trav2 s t a b | a -> s, a t -> b where
  t'2 :: Traversal s t a b
class Trav3 s t a b | a -> s, a t -> b where
  t'3 :: Traversal s t a b
class Trav4 s t a b | a -> s, a t -> b where
  t'4 :: Traversal s t a b
class Trav5 s t a b | a -> s, a t -> b where
  t'5 :: Traversal s t a b
class Trav6 s t a b | a -> s, a t -> b where
  t'6 :: Traversal s t a b
class Trav7 s t a b | a -> s, a t -> b where
  t'7 :: Traversal s t a b
class Trav8 s t a b | a -> s, a t -> b where
  t'8 :: Traversal s t a b
class Trav9 s t a b | a -> s, a t -> b where
  t'9 :: Traversal s t a b

instance Lens1 a a [a] [a] where
  l'1 = lens (\ ~(a:_) -> a ) (\ ~(_:t) a -> a:t )

instance Lens1 a b (Assoc a c) (Assoc b c) where
  l'1 = lens (\(Assoc k _) -> k) (\(Assoc _ a) k -> Assoc k a)
instance Lens2 a b (Assoc c a) (Assoc c b) where
  l'2 = lens (\(Assoc _ a) -> a) (\(Assoc k _) a -> Assoc k a)

instance Lens1 x y (Tuple2 x a)               (Tuple2 y a) where
  l'1 = lens (\ ~(x,_) -> x)               (\ (_,a) y -> (y,a))
instance Lens1 x y (Tuple3 x a b)             (Tuple3 y a b) where
  l'1 = lens (\ ~(x,_,_) -> x)             (\ (_,a,b) y -> (y,a,b))
instance Lens1 x y (Tuple4 x a b c)           (Tuple4 y a b c) where
  l'1 = lens (\ ~(x,_,_,_) -> x)           (\ (_,a,b,c) y -> (y,a,b,c))
instance Lens1 x y (Tuple5 x a b c d)         (Tuple5 y a b c d) where
  l'1 = lens (\ ~(x,_,_,_,_) -> x)         (\ (_,a,b,c,d) y -> (y,a,b,c,d))
instance Lens1 x y (Tuple6 x a b c d e)       (Tuple6 y a b c d e) where
  l'1 = lens (\ ~(x,_,_,_,_,_) -> x)       (\ ~(_,a,b,c,d,e) y -> (y,a,b,c,d,e))
instance Lens1 x y (Tuple7 x a b c d e f)     (Tuple7 y a b c d e f) where
  l'1 = lens (\ ~(x,_,_,_,_,_,_) -> x)     (\ ~(_,a,b,c,d,e,f) y -> (y,a,b,c,d,e,f))
instance Lens1 x y (Tuple8 x a b c d e f g)   (Tuple8 y a b c d e f g) where
  l'1 = lens (\ ~(x,_,_,_,_,_,_,_) -> x)   (\ ~(_,a,b,c,d,e,f,g) y -> (y,a,b,c,d,e,f,g))
instance Lens1 x y (Tuple9 x a b c d e f g h) (Tuple9 y a b c d e f g h) where
  l'1 = lens (\ ~(x,_,_,_,_,_,_,_,_) -> x) (\ ~(_,a,b,c,d,e,f,g,h) y -> (y,a,b,c,d,e,f,g,h))

instance Lens2 x y (Tuple2 a x)               (Tuple2 a y) where
  l'2 = lens (\ ~(_,x) -> x)               (\ ~(a,_) y -> (a,y))
instance Lens2 x y (Tuple3 a x b)             (Tuple3 a y b) where
  l'2 = lens (\ ~(_,x,_) -> x)             (\ ~(a,_,b) y -> (a,y,b))
instance Lens2 x y (Tuple4 a x b c)           (Tuple4 a y b c) where
  l'2 = lens (\ ~(_,x,_,_) -> x)           (\ ~(a,_,b,c) y -> (a,y,b,c))
instance Lens2 x y (Tuple5 a x b c d)         (Tuple5 a y b c d) where
  l'2 = lens (\ ~(_,x,_,_,_) -> x)         (\ ~(a,_,b,c,d) y -> (a,y,b,c,d))
instance Lens2 x y (Tuple6 a x b c d e)       (Tuple6 a y b c d e) where
  l'2 = lens (\ ~(_,x,_,_,_,_) -> x)       (\ ~(a,_,b,c,d,e) y -> (a,y,b,c,d,e))
instance Lens2 x y (Tuple7 a x b c d e f)     (Tuple7 a y b c d e f) where
  l'2 = lens (\ ~(_,x,_,_,_,_,_) -> x)     (\ ~(a,_,b,c,d,e,f) y -> (a,y,b,c,d,e,f))
instance Lens2 x y (Tuple8 a x b c d e f g)   (Tuple8 a y b c d e f g) where
  l'2 = lens (\ ~(_,x,_,_,_,_,_,_) -> x)   (\ ~(a,_,b,c,d,e,f,g) y -> (a,y,b,c,d,e,f,g))
instance Lens2 x y (Tuple9 a x b c d e f g h) (Tuple9 a y b c d e f g h) where
  l'2 = lens (\ ~(_,x,_,_,_,_,_,_,_) -> x) (\ ~(a,_,b,c,d,e,f,g,h) y -> (a,y,b,c,d,e,f,g,h))

instance Lens3 x y (Tuple3 a b x)             (Tuple3 a b y) where
  l'3 = lens (\ ~(_,_,x) -> x)             (\ ~(a,b,_) y -> (a,b,y))
instance Lens3 x y (Tuple4 a b x c)           (Tuple4 a b y c) where
  l'3 = lens (\ ~(_,_,x,_) -> x)           (\ ~(a,b,_,c) y -> (a,b,y,c))
instance Lens3 x y (Tuple5 a b x c d)         (Tuple5 a b y c d) where
  l'3 = lens (\ ~(_,_,x,_,_) -> x)         (\ ~(a,b,_,c,d) y -> (a,b,y,c,d))
instance Lens3 x y (Tuple6 a b x c d e)       (Tuple6 a b y c d e) where
  l'3 = lens (\ ~(_,_,x,_,_,_) -> x)       (\ ~(a,b,_,c,d,e) y -> (a,b,y,c,d,e))
instance Lens3 x y (Tuple7 a b x c d e f)     (Tuple7 a b y c d e f) where
  l'3 = lens (\ ~(_,_,x,_,_,_,_) -> x)     (\ ~(a,b,_,c,d,e,f) y -> (a,b,y,c,d,e,f))
instance Lens3 x y (Tuple8 a b x c d e f g)   (Tuple8 a b y c d e f g) where
  l'3 = lens (\ ~(_,_,x,_,_,_,_,_) -> x)   (\ ~(a,b,_,c,d,e,f,g) y -> (a,b,y,c,d,e,f,g))
instance Lens3 x y (Tuple9 a b x c d e f g h) (Tuple9 a b y c d e f g h) where
  l'3 = lens (\ ~(_,_,x,_,_,_,_,_,_) -> x) (\ ~(a,b,_,c,d,e,f,g,h) y -> (a,b,y,c,d,e,f,g,h))

instance Lens4 x y (Tuple4 a b c x)           (Tuple4 a b c y) where
  l'4 = lens (\ ~(_,_,_,x) -> x)           (\ ~(a,b,c,_) y -> (a,b,c,y))
instance Lens4 x y (Tuple5 a b c x d)         (Tuple5 a b c y d) where
  l'4 = lens (\ ~(_,_,_,x,_) -> x)         (\ ~(a,b,c,_,d) y -> (a,b,c,y,d))
instance Lens4 x y (Tuple6 a b c x d e)       (Tuple6 a b c y d e) where
  l'4 = lens (\ ~(_,_,_,x,_,_) -> x)       (\ ~(a,b,c,_,d,e) y -> (a,b,c,y,d,e))
instance Lens4 x y (Tuple7 a b c x d e f)     (Tuple7 a b c y d e f) where
  l'4 = lens (\ ~(_,_,_,x,_,_,_) -> x)     (\ ~(a,b,c,_,d,e,f) y -> (a,b,c,y,d,e,f))
instance Lens4 x y (Tuple8 a b c x d e f g)   (Tuple8 a b c y d e f g) where
  l'4 = lens (\ ~(_,_,_,x,_,_,_,_) -> x)   (\ ~(a,b,c,_,d,e,f,g) y -> (a,b,c,y,d,e,f,g))
instance Lens4 x y (Tuple9 a b c x d e f g h) (Tuple9 a b c y d e f g h) where
  l'4 = lens (\ ~(_,_,_,x,_,_,_,_,_) -> x) (\ ~(a,b,c,_,d,e,f,g,h) y -> (a,b,c,y,d,e,f,g,h))

instance Lens5 x y (Tuple5 a b c d x)         (Tuple5 a b c d y) where
  l'5 = lens (\ ~(_,_,_,_,x) -> x)         (\ ~(a,b,c,d,_) y -> (a,b,c,d,y))
instance Lens5 x y (Tuple6 a b c d x e)       (Tuple6 a b c d y e) where
  l'5 = lens (\ ~(_,_,_,_,x,_) -> x)       (\ ~(a,b,c,d,_,e) y -> (a,b,c,d,y,e))
instance Lens5 x y (Tuple7 a b c d x e f)     (Tuple7 a b c d y e f) where
  l'5 = lens (\ ~(_,_,_,_,x,_,_) -> x)     (\ ~(a,b,c,d,_,e,f) y -> (a,b,c,d,y,e,f))
instance Lens5 x y (Tuple8 a b c d x e f g)   (Tuple8 a b c d y e f g) where
  l'5 = lens (\ ~(_,_,_,_,x,_,_,_) -> x)   (\ ~(a,b,c,d,_,e,f,g) y -> (a,b,c,d,y,e,f,g))
instance Lens5 x y (Tuple9 a b c d x e f g h) (Tuple9 a b c d y e f g h) where
  l'5 = lens (\ ~(_,_,_,_,x,_,_,_,_) -> x) (\ ~(a,b,c,d,_,e,f,g,h) y -> (a,b,c,d,y,e,f,g,h))

instance Lens6 x y (Tuple6 a b c d e x)       (Tuple6 a b c d e y) where
  l'6 = lens (\ ~(_,_,_,_,_,x) -> x)       (\ ~(a,b,c,d,e,_) y -> (a,b,c,d,e,y))
instance Lens6 x y (Tuple7 a b c d e x f)     (Tuple7 a b c d e y f) where
  l'6 = lens (\ ~(_,_,_,_,_,x,_) -> x)     (\ ~(a,b,c,d,e,_,f) y -> (a,b,c,d,e,y,f))
instance Lens6 x y (Tuple8 a b c d e x f g)   (Tuple8 a b c d e y f g) where
  l'6 = lens (\ ~(_,_,_,_,_,x,_,_) -> x)   (\ ~(a,b,c,d,e,_,f,g) y -> (a,b,c,d,e,y,f,g))
instance Lens6 x y (Tuple9 a b c d e x f g h) (Tuple9 a b c d e y f g h) where
  l'6 = lens (\ ~(_,_,_,_,_,x,_,_,_) -> x) (\ ~(a,b,c,d,e,_,f,g,h) y -> (a,b,c,d,e,y,f,g,h))

instance Lens7 x y (Tuple7 a b c d e f x)     (Tuple7 a b c d e f y) where
  l'7 = lens (\ ~(_,_,_,_,_,_,x) -> x)     (\ ~(a,b,c,d,e,f,_) y -> (a,b,c,d,e,f,y))
instance Lens7 x y (Tuple8 a b c d e f x g)   (Tuple8 a b c d e f y g) where
  l'7 = lens (\ ~(_,_,_,_,_,_,x,_) -> x)   (\ ~(a,b,c,d,e,f,_,g) y -> (a,b,c,d,e,f,y,g))
instance Lens7 x y (Tuple9 a b c d e f x g h) (Tuple9 a b c d e f y g h) where
  l'7 = lens (\ ~(_,_,_,_,_,_,x,_,_) -> x) (\ ~(a,b,c,d,e,f,_,g,h) y -> (a,b,c,d,e,f,y,g,h))

instance Lens8 x y (Tuple8 a b c d e f g x)   (Tuple8 a b c d e f g y) where
  l'8 = lens (\ ~(_,_,_,_,_,_,_,x) -> x)   (\ ~(a,b,c,d,e,f,g,_) y -> (a,b,c,d,e,f,g,y))
instance Lens8 x y (Tuple9 a b c d e f g x h) (Tuple9 a b c d e f g y h) where
  l'8 = lens (\ ~(_,_,_,_,_,_,_,x,_) -> x) (\ ~(a,b,c,d,e,f,g,_,h) y -> (a,b,c,d,e,f,g,y,h))

instance Lens9 x y (Tuple9 a b c d e f g h x) (Tuple9 a b c d e f g h y) where
  l'9 = lens (\ ~(_,_,_,_,_,_,_,_,x) -> x) (\ ~(a,b,c,d,e,f,g,h,_) y -> (a,b,c,d,e,f,g,h,y))

instance Trav1 a a [a] [a] where
  t'1 k (x:l) = (:l)<$>k x ; t'1 _ [] = pure []
instance Trav2 a b (Maybe a) (Maybe b) where
  t'2 k (Just x) = Just<$>k x; t'2 _ Nothing = pure Nothing

instance Trav1 a b (a:+:c) (b:+:c) where
  t'1 k (Left x) = Left<$>k x; t'1 _ (Right x) = pure (Right x)
instance Trav2 a b (c:+:a) (c:+:b) where
  t'2 k (Right x) = Right<$>k x; t'2 _ (Left x) = pure (Left x)

instance Trav1 x y (Union3 x a b) (Union3 y a b) where
  t'1 k (U3_1 x) = U3_1<$>k x
  t'1 _ (U3_2 x) = pure (U3_2 x)
  t'1 _ (U3_3 x) = pure (U3_3 x)
instance Trav1 x y (Union4 x a b c) (Union4 y a b c) where
  t'1 k (U4_1 x) = U4_1<$>k x
  t'1 _ (U4_2 x) = pure (U4_2 x)
  t'1 _ (U4_3 x) = pure (U4_3 x)
  t'1 _ (U4_4 x) = pure (U4_4 x)
instance Trav1 x y (Union5 x a b c d) (Union5 y a b c d) where
  t'1 k (U5_1 x) = U5_1<$>k x
  t'1 _ (U5_2 x) = pure (U5_2 x)
  t'1 _ (U5_3 x) = pure (U5_3 x)
  t'1 _ (U5_4 x) = pure (U5_4 x)
  t'1 _ (U5_5 x) = pure (U5_5 x)
instance Trav1 x y (Union6 x a b c d e) (Union6 y a b c d e) where
  t'1 k (U6_1 x) = U6_1<$>k x
  t'1 _ (U6_2 x) = pure (U6_2 x)
  t'1 _ (U6_3 x) = pure (U6_3 x)
  t'1 _ (U6_4 x) = pure (U6_4 x)
  t'1 _ (U6_5 x) = pure (U6_5 x)
  t'1 _ (U6_6 x) = pure (U6_6 x)
instance Trav1 x y (Union7 x a b c d e f) (Union7 y a b c d e f) where
  t'1 k (U7_1 x) = U7_1<$>k x
  t'1 _ (U7_2 x) = pure (U7_2 x)
  t'1 _ (U7_3 x) = pure (U7_3 x)
  t'1 _ (U7_4 x) = pure (U7_4 x)
  t'1 _ (U7_5 x) = pure (U7_5 x)
  t'1 _ (U7_6 x) = pure (U7_6 x)
  t'1 _ (U7_7 x) = pure (U7_7 x)
instance Trav1 x y (Union8 x a b c d e f g) (Union8 y a b c d e f g) where
  t'1 k (U8_1 x) = U8_1<$>k x
  t'1 _ (U8_2 x) = pure (U8_2 x)
  t'1 _ (U8_3 x) = pure (U8_3 x)
  t'1 _ (U8_4 x) = pure (U8_4 x)
  t'1 _ (U8_5 x) = pure (U8_5 x)
  t'1 _ (U8_6 x) = pure (U8_6 x)
  t'1 _ (U8_7 x) = pure (U8_7 x)
  t'1 _ (U8_8 x) = pure (U8_8 x)
instance Trav1 x y (Union9 x a b c d e f g h) (Union9 y a b c d e f g h) where
  t'1 k (U9_1 x) = U9_1<$>k x
  t'1 _ (U9_2 x) = pure (U9_2 x)
  t'1 _ (U9_3 x) = pure (U9_3 x)
  t'1 _ (U9_4 x) = pure (U9_4 x)
  t'1 _ (U9_5 x) = pure (U9_5 x)
  t'1 _ (U9_6 x) = pure (U9_6 x)
  t'1 _ (U9_7 x) = pure (U9_7 x)
  t'1 _ (U9_8 x) = pure (U9_8 x)
  t'1 _ (U9_9 x) = pure (U9_9 x)

instance Trav2 x y (Union3 a x b) (Union3 a y b) where
  t'2 k (U3_2 x) = U3_2<$>k x
  t'2 _ (U3_1 x) = pure (U3_1 x)
  t'2 _ (U3_3 x) = pure (U3_3 x)
instance Trav2 x y (Union4 a x b c) (Union4 a y b c) where
  t'2 k (U4_2 x) = U4_2<$>k x
  t'2 _ (U4_1 x) = pure (U4_1 x)
  t'2 _ (U4_3 x) = pure (U4_3 x)
  t'2 _ (U4_4 x) = pure (U4_4 x)
instance Trav2 x y (Union5 a x b c d) (Union5 a y b c d) where
  t'2 k (U5_2 x) = U5_2<$>k x
  t'2 _ (U5_1 x) = pure (U5_1 x)
  t'2 _ (U5_3 x) = pure (U5_3 x)
  t'2 _ (U5_4 x) = pure (U5_4 x)
  t'2 _ (U5_5 x) = pure (U5_5 x)
instance Trav2 x y (Union6 a x b c d e) (Union6 a y b c d e) where
  t'2 k (U6_2 x) = U6_2<$>k x
  t'2 _ (U6_1 x) = pure (U6_1 x)
  t'2 _ (U6_3 x) = pure (U6_3 x)
  t'2 _ (U6_4 x) = pure (U6_4 x)
  t'2 _ (U6_5 x) = pure (U6_5 x)
  t'2 _ (U6_6 x) = pure (U6_6 x)
instance Trav2 x y (Union7 a x b c d e f) (Union7 a y b c d e f) where
  t'2 k (U7_2 x) = U7_2<$>k x
  t'2 _ (U7_1 x) = pure (U7_1 x)
  t'2 _ (U7_3 x) = pure (U7_3 x)
  t'2 _ (U7_4 x) = pure (U7_4 x)
  t'2 _ (U7_5 x) = pure (U7_5 x)
  t'2 _ (U7_6 x) = pure (U7_6 x)
  t'2 _ (U7_7 x) = pure (U7_7 x)
instance Trav2 x y (Union8 a x b c d e f g) (Union8 a y b c d e f g) where
  t'2 k (U8_2 x) = U8_2<$>k x
  t'2 _ (U8_1 x) = pure (U8_1 x)
  t'2 _ (U8_3 x) = pure (U8_3 x)
  t'2 _ (U8_4 x) = pure (U8_4 x)
  t'2 _ (U8_5 x) = pure (U8_5 x)
  t'2 _ (U8_6 x) = pure (U8_6 x)
  t'2 _ (U8_7 x) = pure (U8_7 x)
  t'2 _ (U8_8 x) = pure (U8_8 x)
instance Trav2 x y (Union9 a x b c d e f g h) (Union9 a y b c d e f g h) where
  t'2 k (U9_2 x) = U9_2<$>k x
  t'2 _ (U9_1 x) = pure (U9_1 x)
  t'2 _ (U9_3 x) = pure (U9_3 x)
  t'2 _ (U9_4 x) = pure (U9_4 x)
  t'2 _ (U9_5 x) = pure (U9_5 x)
  t'2 _ (U9_6 x) = pure (U9_6 x)
  t'2 _ (U9_7 x) = pure (U9_7 x)
  t'2 _ (U9_8 x) = pure (U9_8 x)
  t'2 _ (U9_9 x) = pure (U9_9 x)

instance Trav3 x y (Union3 a b x) (Union3 a b y) where
  t'3 k (U3_3 x) = U3_3<$>k x
  t'3 _ (U3_1 x) = pure (U3_1 x)
  t'3 _ (U3_2 x) = pure (U3_2 x)
instance Trav3 x y (Union4 a b x c) (Union4 a b y c) where
  t'3 k (U4_3 x) = U4_3<$>k x
  t'3 _ (U4_1 x) = pure (U4_1 x)
  t'3 _ (U4_2 x) = pure (U4_2 x)
  t'3 _ (U4_4 x) = pure (U4_4 x)
instance Trav3 x y (Union5 a b x c d) (Union5 a b y c d) where
  t'3 k (U5_3 x) = U5_3<$>k x
  t'3 _ (U5_1 x) = pure (U5_1 x)
  t'3 _ (U5_2 x) = pure (U5_2 x)
  t'3 _ (U5_4 x) = pure (U5_4 x)
  t'3 _ (U5_5 x) = pure (U5_5 x)
instance Trav3 x y (Union6 a b x c d e) (Union6 a b y c d e) where
  t'3 k (U6_3 x) = U6_3<$>k x
  t'3 _ (U6_1 x) = pure (U6_1 x)
  t'3 _ (U6_2 x) = pure (U6_2 x)
  t'3 _ (U6_4 x) = pure (U6_4 x)
  t'3 _ (U6_5 x) = pure (U6_5 x)
  t'3 _ (U6_6 x) = pure (U6_6 x)
instance Trav3 x y (Union7 a b x c d e f) (Union7 a b y c d e f) where
  t'3 k (U7_3 x) = U7_3<$>k x
  t'3 _ (U7_1 x) = pure (U7_1 x)
  t'3 _ (U7_2 x) = pure (U7_2 x)
  t'3 _ (U7_4 x) = pure (U7_4 x)
  t'3 _ (U7_5 x) = pure (U7_5 x)
  t'3 _ (U7_6 x) = pure (U7_6 x)
  t'3 _ (U7_7 x) = pure (U7_7 x)
instance Trav3 x y (Union8 a b x c d e f g) (Union8 a b y c d e f g) where
  t'3 k (U8_3 x) = U8_3<$>k x
  t'3 _ (U8_1 x) = pure (U8_1 x)
  t'3 _ (U8_2 x) = pure (U8_2 x)
  t'3 _ (U8_4 x) = pure (U8_4 x)
  t'3 _ (U8_5 x) = pure (U8_5 x)
  t'3 _ (U8_6 x) = pure (U8_6 x)
  t'3 _ (U8_7 x) = pure (U8_7 x)
  t'3 _ (U8_8 x) = pure (U8_8 x)
instance Trav3 x y (Union9 a b x c d e f g h) (Union9 a b y c d e f g h) where
  t'3 k (U9_3 x) = U9_3<$>k x
  t'3 _ (U9_1 x) = pure (U9_1 x)
  t'3 _ (U9_2 x) = pure (U9_2 x)
  t'3 _ (U9_4 x) = pure (U9_4 x)
  t'3 _ (U9_5 x) = pure (U9_5 x)
  t'3 _ (U9_6 x) = pure (U9_6 x)
  t'3 _ (U9_7 x) = pure (U9_7 x)
  t'3 _ (U9_8 x) = pure (U9_8 x)
  t'3 _ (U9_9 x) = pure (U9_9 x)

instance Trav4 x y (Union4 a b c x) (Union4 a b c y) where
  t'4 k (U4_4 x) = U4_4<$>k x
  t'4 _ (U4_1 x) = pure (U4_1 x)
  t'4 _ (U4_2 x) = pure (U4_2 x)
  t'4 _ (U4_3 x) = pure (U4_3 x)
instance Trav4 x y (Union5 a b c x d) (Union5 a b c y d) where
  t'4 k (U5_4 x) = U5_4<$>k x
  t'4 _ (U5_1 x) = pure (U5_1 x)
  t'4 _ (U5_2 x) = pure (U5_2 x)
  t'4 _ (U5_3 x) = pure (U5_3 x)
  t'4 _ (U5_5 x) = pure (U5_5 x)
instance Trav4 x y (Union6 a b c x d e) (Union6 a b c y d e) where
  t'4 k (U6_4 x) = U6_4<$>k x
  t'4 _ (U6_1 x) = pure (U6_1 x)
  t'4 _ (U6_2 x) = pure (U6_2 x)
  t'4 _ (U6_3 x) = pure (U6_3 x)
  t'4 _ (U6_5 x) = pure (U6_5 x)
  t'4 _ (U6_6 x) = pure (U6_6 x)
instance Trav4 x y (Union7 a b c x d e f) (Union7 a b c y d e f) where
  t'4 k (U7_4 x) = U7_4<$>k x
  t'4 _ (U7_1 x) = pure (U7_1 x)
  t'4 _ (U7_2 x) = pure (U7_2 x)
  t'4 _ (U7_3 x) = pure (U7_3 x)
  t'4 _ (U7_5 x) = pure (U7_5 x)
  t'4 _ (U7_6 x) = pure (U7_6 x)
  t'4 _ (U7_7 x) = pure (U7_7 x)
instance Trav4 x y (Union8 a b c x d e f g) (Union8 a b c y d e f g) where
  t'4 k (U8_4 x) = U8_4<$>k x
  t'4 _ (U8_1 x) = pure (U8_1 x)
  t'4 _ (U8_2 x) = pure (U8_2 x)
  t'4 _ (U8_3 x) = pure (U8_3 x)
  t'4 _ (U8_5 x) = pure (U8_5 x)
  t'4 _ (U8_6 x) = pure (U8_6 x)
  t'4 _ (U8_7 x) = pure (U8_7 x)
  t'4 _ (U8_8 x) = pure (U8_8 x)
instance Trav4 x y (Union9 a b c x d e f g h) (Union9 a b c y d e f g h) where
  t'4 k (U9_4 x) = U9_4<$>k x
  t'4 _ (U9_1 x) = pure (U9_1 x)
  t'4 _ (U9_2 x) = pure (U9_2 x)
  t'4 _ (U9_3 x) = pure (U9_3 x)
  t'4 _ (U9_5 x) = pure (U9_5 x)
  t'4 _ (U9_6 x) = pure (U9_6 x)
  t'4 _ (U9_7 x) = pure (U9_7 x)
  t'4 _ (U9_8 x) = pure (U9_8 x)
  t'4 _ (U9_9 x) = pure (U9_9 x)

instance Trav5 x y (Union5 a b c d x) (Union5 a b c d y) where
  t'5 k (U5_5 x) = U5_5<$>k x
  t'5 _ (U5_1 x) = pure (U5_1 x)
  t'5 _ (U5_2 x) = pure (U5_2 x)
  t'5 _ (U5_3 x) = pure (U5_3 x)
  t'5 _ (U5_4 x) = pure (U5_4 x)
instance Trav5 x y (Union6 a b c d x e) (Union6 a b c d y e) where
  t'5 k (U6_5 x) = U6_5<$>k x
  t'5 _ (U6_1 x) = pure (U6_1 x)
  t'5 _ (U6_2 x) = pure (U6_2 x)
  t'5 _ (U6_3 x) = pure (U6_3 x)
  t'5 _ (U6_4 x) = pure (U6_4 x)
  t'5 _ (U6_6 x) = pure (U6_6 x)
instance Trav5 x y (Union7 a b c d x e f) (Union7 a b c d y e f) where
  t'5 k (U7_5 x) = U7_5<$>k x
  t'5 _ (U7_1 x) = pure (U7_1 x)
  t'5 _ (U7_2 x) = pure (U7_2 x)
  t'5 _ (U7_3 x) = pure (U7_3 x)
  t'5 _ (U7_4 x) = pure (U7_4 x)
  t'5 _ (U7_6 x) = pure (U7_6 x)
  t'5 _ (U7_7 x) = pure (U7_7 x)
instance Trav5 x y (Union8 a b c d x e f g) (Union8 a b c d y e f g) where
  t'5 k (U8_5 x) = U8_5<$>k x
  t'5 _ (U8_1 x) = pure (U8_1 x)
  t'5 _ (U8_2 x) = pure (U8_2 x)
  t'5 _ (U8_3 x) = pure (U8_3 x)
  t'5 _ (U8_4 x) = pure (U8_4 x)
  t'5 _ (U8_6 x) = pure (U8_6 x)
  t'5 _ (U8_7 x) = pure (U8_7 x)
  t'5 _ (U8_8 x) = pure (U8_8 x)
instance Trav5 x y (Union9 a b c d x e f g h) (Union9 a b c d y e f g h) where
  t'5 k (U9_5 x) = U9_5<$>k x
  t'5 _ (U9_1 x) = pure (U9_1 x)
  t'5 _ (U9_2 x) = pure (U9_2 x)
  t'5 _ (U9_3 x) = pure (U9_3 x)
  t'5 _ (U9_4 x) = pure (U9_4 x)
  t'5 _ (U9_6 x) = pure (U9_6 x)
  t'5 _ (U9_7 x) = pure (U9_7 x)
  t'5 _ (U9_8 x) = pure (U9_8 x)
  t'5 _ (U9_9 x) = pure (U9_9 x)

instance Trav6 x y (Union6 a b c d e x) (Union6 a b c d e y) where
  t'6 k (U6_6 x) = U6_6<$>k x
  t'6 _ (U6_1 x) = pure (U6_1 x)
  t'6 _ (U6_2 x) = pure (U6_2 x)
  t'6 _ (U6_3 x) = pure (U6_3 x)
  t'6 _ (U6_4 x) = pure (U6_4 x)
  t'6 _ (U6_5 x) = pure (U6_5 x)
instance Trav6 x y (Union7 a b c d e x f) (Union7 a b c d e y f) where
  t'6 k (U7_6 x) = U7_6<$>k x
  t'6 _ (U7_1 x) = pure (U7_1 x)
  t'6 _ (U7_2 x) = pure (U7_2 x)
  t'6 _ (U7_3 x) = pure (U7_3 x)
  t'6 _ (U7_4 x) = pure (U7_4 x)
  t'6 _ (U7_5 x) = pure (U7_5 x)
  t'6 _ (U7_7 x) = pure (U7_7 x)
instance Trav6 x y (Union8 a b c d e x f g) (Union8 a b c d e y f g) where
  t'6 k (U8_6 x) = U8_6<$>k x
  t'6 _ (U8_1 x) = pure (U8_1 x)
  t'6 _ (U8_2 x) = pure (U8_2 x)
  t'6 _ (U8_3 x) = pure (U8_3 x)
  t'6 _ (U8_4 x) = pure (U8_4 x)
  t'6 _ (U8_5 x) = pure (U8_5 x)
  t'6 _ (U8_7 x) = pure (U8_7 x)
  t'6 _ (U8_8 x) = pure (U8_8 x)
instance Trav6 x y (Union9 a b c d e x f g h) (Union9 a b c d e y f g h) where
  t'6 k (U9_6 x) = U9_6<$>k x
  t'6 _ (U9_1 x) = pure (U9_1 x)
  t'6 _ (U9_2 x) = pure (U9_2 x)
  t'6 _ (U9_3 x) = pure (U9_3 x)
  t'6 _ (U9_4 x) = pure (U9_4 x)
  t'6 _ (U9_5 x) = pure (U9_5 x)
  t'6 _ (U9_7 x) = pure (U9_7 x)
  t'6 _ (U9_8 x) = pure (U9_8 x)
  t'6 _ (U9_9 x) = pure (U9_9 x)

instance Trav7 x y (Union7 a b c d e f x) (Union7 a b c d e f y) where
  t'7 k (U7_7 x) = U7_7<$>k x
  t'7 _ (U7_1 x) = pure (U7_1 x)
  t'7 _ (U7_2 x) = pure (U7_2 x)
  t'7 _ (U7_3 x) = pure (U7_3 x)
  t'7 _ (U7_4 x) = pure (U7_4 x)
  t'7 _ (U7_5 x) = pure (U7_5 x)
  t'7 _ (U7_6 x) = pure (U7_6 x)
instance Trav7 x y (Union8 a b c d e f x g) (Union8 a b c d e f y g) where
  t'7 k (U8_7 x) = U8_7<$>k x
  t'7 _ (U8_1 x) = pure (U8_1 x)
  t'7 _ (U8_2 x) = pure (U8_2 x)
  t'7 _ (U8_3 x) = pure (U8_3 x)
  t'7 _ (U8_4 x) = pure (U8_4 x)
  t'7 _ (U8_5 x) = pure (U8_5 x)
  t'7 _ (U8_6 x) = pure (U8_6 x)
  t'7 _ (U8_8 x) = pure (U8_8 x)
instance Trav7 x y (Union9 a b c d e f x g h) (Union9 a b c d e f y g h) where
  t'7 k (U9_7 x) = U9_7<$>k x
  t'7 _ (U9_1 x) = pure (U9_1 x)
  t'7 _ (U9_2 x) = pure (U9_2 x)
  t'7 _ (U9_3 x) = pure (U9_3 x)
  t'7 _ (U9_4 x) = pure (U9_4 x)
  t'7 _ (U9_5 x) = pure (U9_5 x)
  t'7 _ (U9_6 x) = pure (U9_6 x)
  t'7 _ (U9_8 x) = pure (U9_8 x)
  t'7 _ (U9_9 x) = pure (U9_9 x)

instance Trav8 x y (Union8 a b c d e f g x) (Union8 a b c d e f g y) where
  t'8 k (U8_8 x) = U8_8<$>k x
  t'8 _ (U8_1 x) = pure (U8_1 x)
  t'8 _ (U8_2 x) = pure (U8_2 x)
  t'8 _ (U8_3 x) = pure (U8_3 x)
  t'8 _ (U8_4 x) = pure (U8_4 x)
  t'8 _ (U8_5 x) = pure (U8_5 x)
  t'8 _ (U8_6 x) = pure (U8_6 x)
  t'8 _ (U8_7 x) = pure (U8_7 x)
instance Trav8 x y (Union9 a b c d e f g x h) (Union9 a b c d e f g y h) where
  t'8 k (U9_8 x) = U9_8<$>k x
  t'8 _ (U9_1 x) = pure (U9_1 x)
  t'8 _ (U9_2 x) = pure (U9_2 x)
  t'8 _ (U9_3 x) = pure (U9_3 x)
  t'8 _ (U9_4 x) = pure (U9_4 x)
  t'8 _ (U9_5 x) = pure (U9_5 x)
  t'8 _ (U9_6 x) = pure (U9_6 x)
  t'8 _ (U9_7 x) = pure (U9_7 x)
  t'8 _ (U9_9 x) = pure (U9_9 x)

instance Trav9 x y (Union9 a b c d e f g h x) (Union9 a b c d e f g h y) where
  t'9 k (U9_9 x) = U9_9<$>k x
  t'9 _ (U9_1 x) = pure (U9_1 x)
  t'9 _ (U9_2 x) = pure (U9_2 x)
  t'9 _ (U9_3 x) = pure (U9_3 x)
  t'9 _ (U9_4 x) = pure (U9_4 x)
  t'9 _ (U9_5 x) = pure (U9_5 x)
  t'9 _ (U9_6 x) = pure (U9_6 x)
  t'9 _ (U9_7 x) = pure (U9_7 x)
  t'9 _ (U9_8 x) = pure (U9_8 x)

class Compound a b s t | s -> a, b s -> t where
  each :: Traversal a b s t
instance Compound a b (a,a) (b,b) where
  each k (a,a') = (,)<$>k a<*>k a'
instance Compound a b (a,a,a) (b,b,b) where
  each k (a,a',a'') = (,,)<$>k a<*>k a'<*>k a''
instance Compound a b (a:+:a) (b:+:b) where
  each k = map Left . k <|> map Right . k
i'list :: [a] :<->: (():+:(a:*:[a]))
i'list = iso (\l -> case l of
                [] -> Left ()
                (x:t) -> Right (x,t)) (const [] <|> uncurry (:))

t'head :: Traversal' [a] a
t'head = t'1
t'tail :: Traversal' [a] [a]
t'tail = i'list.t'2.l'2

mapping :: (Functor f,Functor f') => Iso s t a b -> Iso (f s) (f' t) (f a) (f' b)
mapping (isoT -> IsoT u v) = map u `dimap` map (map v)
mapping' :: Functor f => Iso s t a b -> Iso (f s) (f t) (f a) (f b)
mapping' = mapping
promapping :: Bifunctor f => Iso s t a b -> Iso (f t x) (f s y) (f b x) (f a y)
promapping (isoT -> IsoT u v) = dimap v id`dimap` map (dimap u id)
-- ^promapping :: Bifunctor f => Iso' a b -> Iso' (f a c) (f b c)
applying :: Applicative f => Lens s t a b -> Lens (f s) (f t) (f a) (f b)
applying l = lens _to _from
  where _to = map (by l) ; _from = flip $ liftA2 (set l) 

class Isomorphic b a t s | t -> b, t a -> s where
  i'_ :: Iso s t a b
instance Isomorphic a b (Id a) (Id b) where
  i'_ = iso Id getId
instance Isomorphic [a] [b] (OrdList a) (OrdList b) where
  i'_ = iso OrdList getOrdList
instance Isomorphic a b (Const a c) (Const b c) where
  i'_ = iso Const getConst
instance Isomorphic a b (Dual a) (Dual b) where
  i'_ = iso Dual getDual
instance Isomorphic a b (Product a) (Product b) where
  i'_ = iso Product getProduct
instance Isomorphic a b (Max a) (Max b) where
  i'_ = iso Max getMax
instance Isomorphic (k a a) (k b b) (Endo k a) (Endo k b) where
  i'_ = iso Endo runEndo
instance Isomorphic (f a b) (f c d) (Flip f b a) (Flip f d c) where
  i'_ = iso Flip unFlip
instance Isomorphic Bool Bool (Maybe a) (Maybe Void) where
  i'_ = iso (bool (Just zero) Nothing) (maybe False (const True))
instance Isomorphic (f (g a)) (f' (g' b)) ((f:.:g) a) ((f':.:g') b) where
  i'_ = iso Compose getCompose
instance Isomorphic a b (Void,a) (Void,b) where
  i'_ = iso (zero,) snd
i'Id :: Iso (Id a) (Id b) a b
i'Id = i'_
i'OrdList :: Iso (OrdList a) (OrdList b) [a] [b]
i'OrdList = i'_
i'Dual :: Iso (Dual a) (Dual b) a b
i'Dual = i'_
i'Const :: Iso (Const a c) (Const b c) a b
i'Const = i'_
i'Max :: Iso (Max a) (Max b) a b 
i'Max = i'_
i'Endo :: Iso (Endo k a) (Endo k b) (k a a) (k b b)
i'Endo = i'_ 
i'maybe :: Iso (Maybe Void) (Maybe a) Bool Bool
i'maybe = i'_ 
i'Flip :: Iso (Flip f b a) (Flip f d c) (f a b) (f c d)
i'Flip = i'_
i'Compose :: Iso ((f:.:g) a) ((f':.:g') b) (f (g a)) (f' (g' b))
i'Compose = i'_
i'Backwards :: Iso (Backwards f a) (Backwards g b) (f a) (g b)
i'Backwards = iso Backwards forwards
i'Accum :: Iso (Accum a) (Accum b) (Maybe a) (Maybe b)
i'Accum = iso Accum getAccum

t'Just :: Traversal a b (Maybe a) (Maybe b)
t'Just k (Just a) = Just<$>k a
t'Just _ _ = pure Nothing

l'Just :: a -> Lens a a (Maybe a) (Maybe a)
l'Just _ k (Just a) = Just<$>k a
l'Just a k _ = Just<$>k a

curried :: Iso (a -> b -> c) (a' -> b' -> c') ((a,b) -> c) ((a',b') -> c')
curried = iso curry uncurry

warp2 :: Iso s t a b -> (s -> s -> t) -> (a -> a -> b)
warp2 i f = \a a' -> yb i (by i a`f`by i a')

class IsoFunctor f where
  mapIso :: Iso s t a b -> Iso (f s) (f t) (f a) (f b)
class IsoFunctor2 f where
  mapIso2 :: (a:<->:c) -> (b:<->:d) -> (f a b:<->:f c d)

-- | An infix synonym for 'mapIso2'
(<.>) :: IsoFunctor2 f => (a:<->:c) -> (b:<->:d) -> (f a b:<->:f c d)
(<.>) = mapIso2
infixr 9 <.>

i'pair :: Iso s t a b -> Iso s' t' a' b' -> Iso (s,s') (t,t') (a,a') (b,b')
i'pair i i' = let IsoT u v = isoT i ; IsoT u' v' = isoT i' in iso (u<#>u') (v<#>v')

instance IsoFunctor ((->) a) where mapIso = mapping
instance IsoFunctor2 (->) where mapIso2 i j = promapping i.mapping j
instance IsoFunctor2 (,) where
  mapIso2 = i'pair
instance IsoFunctor2 Either where
  mapIso2 i j = iso (by i ||| by j) (yb i ||| yb j)

adding :: (Num n,Semigroup n) => n -> Iso' n n
adding n = iso (+n) (subtract n)

thunk :: Iso a b (IO a) (IO b)
thunk = iso unsafePerformIO evaluate
chunk :: Bytes:<->:Chunk
chunk = iso toStrict fromStrict

negated :: (Disjonctive a,Disjonctive b) => Iso a b a b
negated = iso negate negate
commuted :: Commutative f => Iso (f a b) (f c d) (f b a) (f d c)
commuted = iso commute commute

newtype Test a = Test (Const Bool a)
               deriving (Semigroup,Monoid,Functor,Unit,SemiApplicative,Applicative)
has :: MonadFix ((,) Bool) => FixFold' a b -> a -> Bool
has l x = fst $ l (\y -> (True,y)) x
