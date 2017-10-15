{-# LANGUAGE NoRebindableSyntax, MultiParamTypeClasses, DefaultSignatures, TupleSections, EmptyDataDecls, CPP #-}
module Algebra.Core(
  -- * Raw data
  Handle,stdin,stdout,stderr,
  Packed(..),
  Chunk,chunkSize,readChunk,writeChunk,readHChunk,writeHChunk,
  Bytes,bytesSize,readBytes,writeBytes,readHBytes,writeHBytes,
  readString,writeString,readHString,writeHString,
  appendString,
  
  -- * Basic union and product types
  Void,(:*:),(:+:),
  Tuple2,Tuple3,Tuple4,Tuple5,Tuple6,Tuple7,Tuple8,Tuple9,
  Union2,Union3(..),Union4(..),Union5(..),Union6(..),Union7(..),Union8(..),Union9(..),
    
  -- * Basic group and ring structure
  -- ** Classes
  Semigroup(..),Monoid(..),Disjonctive(..),Semiring(..),Ring(..),Invertible(..),
  SubSemi(..),
  Unit(..),

  -- ** Common monoids

  -- *** Control monoids
  Endo(..),StrictEndo(..),

  -- *** Meta-monoids
  Dual(..),Product(..),

  -- *** Accumulating monoids
  OrdList(..),Interleave(..),Accum(..),Max(..),Min(..),Id(..),
  
  -- * Fundamental control operations
  Deductive(..),Category(..),(<<<),(>>>),(+++),

  -- ** Splitting and Choosing
  Choice(..),Split(..),
  
  -- * Expression-level type constraints
  Constraint,c'listOf,c'list,c'void,c'int,c'char,c'string,c'float,c'_,
  
  -- * Miscellaneous functions
  const,(&),is,fix,uncurry0,uncurry,uncurry3,uncurry4,

  first,second,

  ifThenElse,bool,extreme,guard,fail,
#if MIN_VERSION_base(4,9,0)
  error,
#endif
  unit,when,unless,

  tailSafe,headDef,fromMaybe,

  rmod,inside,swap,
  
  -- ** Lazily ordering values
  comparing,inOrder,insertOrd,invertOrd,
  Assoc(..),assoc,
  
  -- ** Ranges
  Range(..),

  -- ** Parallel short-circuit evaluation
  amb,unamb,

  -- * The rest is imported from the Prelude
  module Prelude,IsString(..)
  ) where

import Prelude hiding (
  readFile,writeFile,

  Functor(..),Monad(..),

  sequence,mapM,mapM_,sequence_,(=<<),

  map,(++),foldl,foldr,foldr1,scanl,scanr,concat,filter,length,sum,lookup,
  (+),(*),(.),id,const,(-),(/),recip,uncurry,

  or,any,and,all,elem,span,break,splitAt,take,drop,takeWhile,dropWhile,

  until,negate,zipWith,zipWith3,

  minimum,maximum,product
#if !MIN_VERSION_base(4,7,0)
  ,catch
#endif
#if MIN_VERSION_base(4,8,0)
  ,Monoid(..),Applicative(..),(<$>),Foldable(..),Traversable(..)
#endif
#if MIN_VERSION_base(4,9,0)
  ,error
#endif
  )
import Control.Concurrent (killThread,newEmptyMVar,forkIO,putMVar,takeMVar)
import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (stdin,stdout,stderr)
import qualified Prelude as P
import qualified Control.Exception as P
import Data.Tree
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import GHC.IO.Handle (Handle,hGetContents,hPutStr)
import Data.Ord (comparing)
import GHC.Exts (IsString(..))
import Data.Word (Word8)

#if MIN_VERSION_base(4,9,0)
error :: String -> a
error = errorWithoutStackTrace
#endif

type Constraint a = a -> a
c'listOf :: Constraint a -> Constraint [a]
c'listOf _ = c'_
c'list :: Constraint [a]
c'list = c'listOf c'_
c'int :: Constraint Int
c'int = c'_
c'char :: Constraint Char
c'char = c'_
c'string :: Constraint String
c'string = c'_
c'float :: Constraint Float
c'float = c'_
c'couple :: Constraint a -> Constraint b -> Constraint (a,b)
c'couple _ _ = c'_
c'void :: Constraint Void
c'void = c'_
c'_ :: Constraint a
c'_ = id

type Chunk = BSS.ByteString
type Bytes = BSL.ByteString

readBytes :: String -> IO Bytes
readBytes = BSL.readFile
readChunk :: String -> IO Chunk
readChunk = BSS.readFile
readString :: String -> IO String
readString = P.readFile
writeBytes :: String -> Bytes -> IO ()
writeBytes = BSL.writeFile
writeChunk :: String -> Chunk -> IO ()
writeChunk = BSS.writeFile
writeString :: String -> String -> IO ()
writeString = P.writeFile
readHBytes :: Handle -> IO Bytes
readHBytes = BSL.hGetContents
readHChunk :: Handle -> IO Chunk
readHChunk = BSS.hGetContents
readHString :: Handle -> IO String
readHString = hGetContents
writeHBytes :: Handle -> Bytes -> IO ()
writeHBytes = BSL.hPut
writeHChunk :: Handle -> Chunk -> IO ()
writeHChunk = BSS.hPut
writeHString :: Handle -> String -> IO ()
writeHString = hPutStr
appendString :: String -> String -> IO ()
appendString = P.appendFile

chunkSize :: Chunk -> Int
chunkSize = fromIntegral . BSS.length
bytesSize :: Bytes -> Int
bytesSize = fromIntegral . BSL.length

class Packed c where
  pack :: [Word8] -> c
  unpack :: c -> [Word8] 
instance Packed Chunk where pack = BSS.pack; unpack = BSS.unpack
instance Packed Bytes where pack = BSL.pack; unpack = BSL.unpack

data Void
instance Show Void where
  show _ = "<void>"
type a:*:b = (a,b)
type a:+:b = Either a b
type Tuple2 = (,)
type Tuple3 = (,,)
type Tuple4 = (,,,)
type Tuple5 = (,,,,)
type Tuple6 = (,,,,,)
type Tuple7 = (,,,,,,)
type Tuple8 = (,,,,,,,)
type Tuple9 = (,,,,,,,,)
type Union2 = Either
data Union3 a b c              = U3_1 a | U3_2 b | U3_3 c
                               deriving (Eq,Ord,Show,Read)
data Union4 a b c d            = U4_1 a | U4_2 b | U4_3 c | U4_4 d
                               deriving (Eq,Ord,Show,Read)
data Union5 a b c d e          = U5_1 a | U5_2 b | U5_3 c | U5_4 d | U5_5 e
                               deriving (Eq,Ord,Show,Read)
data Union6 a b c d e f        = U6_1 a | U6_2 b | U6_3 c | U6_4 d | U6_5 e | U6_6 f
                               deriving (Eq,Ord,Show,Read)
data Union7 a b c d e f g      = U7_1 a | U7_2 b | U7_3 c | U7_4 d | U7_5 e | U7_6 f | U7_7 g
                               deriving (Eq,Ord,Show,Read)
data Union8 a b c d e f g h    = U8_1 a | U8_2 b | U8_3 c | U8_4 d | U8_5 e | U8_6 f | U8_7 g | U8_8 h
                               deriving (Eq,Ord,Show,Read)
data Union9 a b c d e f g h i  = U9_1 a | U9_2 b | U9_3 c | U9_4 d | U9_5 e | U9_6 f | U9_7 g | U9_8 h | U9_9 i
                               deriving (Eq,Ord,Show,Read)

{-|
The class of all types that have a binary operation. Note that the operation
isn't necesarily commutative (in the case of lists, for example)
-} 
class Semigroup m where
  (+) :: m -> m -> m
  default (+) :: Num m => m -> m -> m
  (+) = (P.+)
infixr 6 +
instance Semigroup Void where _+_ = undefined
instance Semigroup () where _+_ = ()
instance Semigroup Bool where (+) = (||)
instance Semigroup Int
instance Semigroup Integer
instance Semigroup Rational
instance Semigroup Float
instance Semigroup Double
instance Semigroup Bytes where (+) = BSL.append
instance Semigroup Chunk where (+) = BSS.append
instance Semigroup [a] where
  {-# INLINE[2] (+) #-}
  (+) (x:t) = \l -> x:(t+l)
  (+) [] = \l -> l
instance (Semigroup a,Semigroup b) => Semigroup (a:*:b) where ~(a,b) + ~(c,d) = (a+c,b+d)
instance (Semigroup a,Semigroup b,Semigroup c) => Semigroup (a,b,c) where
  ~(a,b,c) + ~(a',b',c') = (a+a',b+b',c+c')
instance (Semigroup a,Semigroup b,Semigroup c,Semigroup d) => Semigroup (a,b,c,d) where
  ~(a,b,c,d) + ~(a',b',c',d') = (a+a',b+b',c+c',d+d')
instance SubSemi b a => Semigroup (a:+:b) where
  Left a+Left b = Left (a+b)
  a+b = Right (from a+from b)
    where from = cast <|> id
instance Semigroup (Maybe a) where
  Nothing + b = b ; a + _ = a
instance Semigroup Ordering where
  EQ + x = x
  x + _ = x

-- |A monoid is a semigroup with a null element such that @zero + a == a + zero == a@
class Semigroup m => Monoid m where
  zero :: m
  default zero :: Num m => m
  zero = 0
instance Monoid Void where zero = undefined
instance Monoid () where zero = ()
instance Monoid Int ; instance Monoid Integer
instance Monoid Rational
instance Monoid Float ; instance Monoid Double
instance Monoid Bytes where zero = BSL.empty
instance Monoid Chunk where zero = BSS.empty
instance Monoid [a] where zero = []
instance (Monoid a,Monoid b) => Monoid (a:*:b) where zero = (zero,zero)
instance (Monoid a,Monoid b,Monoid c) => Monoid (a,b,c) where
  zero = (zero,zero,zero)
instance (Monoid a,Monoid b,Monoid c,Monoid d) => Monoid (a,b,c,d) where
  zero = (zero,zero,zero,zero)
instance (SubSemi b a,Monoid a) => Monoid (a:+:b) where zero = Left zero
instance Monoid Bool where zero = False
instance Monoid (Maybe a) where zero = Nothing
instance Monoid Ordering where zero = EQ
 
class (Semigroup a,Semigroup b) => SubSemi a b where
  cast :: b -> a
instance Monoid a => SubSemi a () where cast _ = zero
instance Monoid a => SubSemi a Void where cast _ = zero
instance Semigroup a => SubSemi a a where cast = id

class Monoid m => Disjonctive m where
  negate :: m -> m
  negate = (zero -)
  (-) :: m -> m -> m
  infixl 5 -
  a-b = a+negate b
instance Disjonctive Int where
  negate = P.negate ; (-) = (P.-)
instance Disjonctive Integer where
  negate = P.negate ; (-) = (P.-)
instance Disjonctive Rational where
  negate = P.negate ; (-) = (P.-)
instance Disjonctive Float where
  negate = P.negate ; (-) = (P.-)
instance Disjonctive Double where
  negate = P.negate ; (-) = (P.-)

instance Disjonctive Bool where
  negate = not
  a - b = not (a==b)
instance Disjonctive Ordering where
  negate LT = GT ; negate GT = LT ; negate EQ = EQ
instance (Disjonctive a,Disjonctive b) => Disjonctive (a:*:b) where
  negate (a,b) = (negate a,negate b)
  (a,b)-(c,d) = (a-c,b-d)

class Monoid m => Semiring m where
  (*) :: m -> m -> m
  default (*) :: Num m => m -> m -> m
  (*) = (P.*)
class Semiring m => Ring m where
  one :: m
  default one :: Num m => m
  one = 1
  
infixl 7 *
instance Semiring Bool where (*) = (&&)
instance Ring Bool where one = True 
instance Semiring Int ; instance Ring Int
instance Semiring Rational ; instance Ring Rational
instance Semiring Integer ; instance Ring Integer
instance Semiring Float ; instance Ring Float
instance Semiring Double ; instance Ring Double

instance Monoid a => Semiring (Maybe a) where
  Just a * Just b = Just (a+b)
  Nothing * a = a
  a * _ = a
instance Monoid a => Ring (Maybe a) where one = zero
instance Monoid a => Semiring [a] where
  (a:as) * (b:bs) = a+b:as*bs
  _ * _ = zero
instance Monoid a => Ring [a] where
  one = zero:one
instance (Semiring a,Semiring b) => Semiring (a:*:b) where
  ~(a,b) * ~(c,d) = (a*c,b*d)
instance (Ring a,Ring b) => Ring (a:*:b) where
  one = (one,one)

class (Ring m,Disjonctive m) => Invertible m where
  recip :: m -> m
  recip = (one /)
  (/) :: m -> m -> m
  infixl 7 /
  a / b = a * recip b
instance Invertible Rational where
  recip = P.recip ; (/) = (P./)
instance Invertible Float where
  recip = P.recip ; (/) = (P./)
instance Invertible Double where
  recip = P.recip ; (/) = (P./)

class Unit f where
  pure :: a -> f a
instance Unit (Either a) where pure = Right
instance Unit Maybe where pure = Just
instance Monoid w => Unit ((,) w) where pure a = (zero,a)
instance Unit ((->) b) where pure = P.const
instance Unit [] where pure a = [a]
instance Unit Tree where pure a = Node a []
instance Unit IO where pure = P.return

class Deductive k where
  (.) :: k b c -> k a b -> k a c
class Deductive k => Category k where
  id :: k a a

instance Deductive (->) where
    (.) = (P..)
instance Category (->) where
  id = P.id
(<<<) :: Category k => k b c -> k a b -> k a c
(<<<) = (.)
(>>>) :: Category k => k a b -> k b c -> k a c
(>>>) = flip (<<<)
infixr 1 >>>,<<<
infixr 9 .

class Category k => Choice k where
  (<|>) :: k a c -> k b c -> k (a:+:b) c
infixr 1 <|>
instance Choice (->) where
  (f <|> _) (Left a) = f a
  (_ <|> g) (Right b) = g b

class Category k => Split k where
  (<#>) :: k a c -> k b d -> k (a,b) (c,d)
infixr 2 <#>
instance Split (->) where f <#> g = \ ~(a,b) -> (f a,g b)

{-| The Product monoid -}
newtype Product a = Product { getProduct :: a }
                  deriving (Eq,Ord,Show)
instance Ring a => Semigroup (Product a) where
  Product a+Product b = Product (a*b) 
instance Ring a => Monoid (Product a) where
  zero = Product one

{-| A monoid on category endomorphisms under composition -}
newtype Endo k a = Endo { runEndo :: k a a }
instance Category k => Semigroup (Endo k a) where Endo f+Endo g = Endo (g . f)
instance Category k => Monoid (Endo k a) where zero = Endo id

newtype StrictEndo a = StrictEndo { runStrictEndo :: a -> a }
instance Semigroup (StrictEndo a) where
  StrictEndo f + StrictEndo g = StrictEndo h
    where h a = let fa = f a in fa `seq` g fa 

{-| A monoid on Maybes, where the sum is the leftmost non-Nothing value. -}
newtype Accum a = Accum { getAccum :: Maybe a }
instance Monoid a => Semigroup (Accum a) where
  Accum Nothing + Accum Nothing = Accum Nothing
  Accum a + Accum b = Accum (Just (from a+from b))
    where from = maybe zero id
instance Monoid a => Monoid (Accum a) where zero = Accum Nothing
instance Unit Accum where pure = Accum . pure

-- |The Identity Functor
newtype Id a = Id { getId :: a }
instance Show a => Show (Id a) where
  show (Id a) = "Id "+show a
instance Unit Id where pure = Id
instance Semigroup (Id a) where a + _ = a                                

{-| The Max monoid, where @(+) =~ max@ -}
newtype Max a = Max { getMax :: a }
              deriving (Eq,Ord,Bounded,Show)
instance Ord a => Semigroup (Max a) where a + b = max a b
instance (Ord a,Bounded a) => Monoid (Max a) where zero = minBound
instance (Ord a,Bounded a) => Semiring (Max a) where a * b = min a b
instance (Ord a,Bounded a) => Ring (Max a) where one = maxBound

{-| The Min monoid, where @(+) =~ min@ -}
newtype Min a = Min { getMin :: a }
              deriving (Eq,Show)
instance Ord a => Ord (Min a) where
  compare (Min a) (Min b) = compare b a
instance Bounded a => Bounded (Min a) where
  minBound = Min maxBound
  maxBound = Min minBound
instance Ord a => Semigroup (Min a) where a + b = max a b
instance (Ord a,Bounded a) => Monoid (Min a) where zero = minBound
instance (Ord a,Bounded a) => Semiring (Min a) where a * b = min a b
instance (Ord a,Bounded a) => Ring (Min a) where one = maxBound

{-| The dual of a monoid is the same as the original, with arguments reversed -}
newtype Dual m = Dual { getDual :: m }
instance Semigroup m => Semigroup (Dual m) where Dual a+Dual b = Dual (b+a)
deriving instance Monoid m => Monoid (Dual m)
instance Semiring m => Semiring (Dual m) where Dual a * Dual b = Dual (b*a)
instance Ring m => Ring (Dual m) where one = Dual one

-- |An ordered list. The semigroup instance merges two lists so that
-- the result remains in ascending order.
newtype OrdList a = OrdList { getOrdList :: [a] }
                  deriving (Eq,Ord,Show)
instance Ord a => Semigroup (OrdList a) where
  OrdList oa + OrdList ob = OrdList (oa ++ ob)
    where (x:xt) ++ (y:yt) = a : c : cs
            where (a,_,z) = inOrder x y
                  ~(c:cs) = if z then xt ++ (y:yt) else (x:xt) ++ yt
          a ++ b = a + b
deriving instance Ord a => Monoid (OrdList a)
deriving instance Unit OrdList

data Assoc k a = Assoc k a
               deriving Show
instance Ord k => Eq (Assoc k a) where
  a == b = compare a b == EQ
instance Ord k => Ord (Assoc k a) where
  compare (Assoc k _) (Assoc k' _) = compare k k'
assoc :: a -> Assoc a a
assoc a = Assoc a a

inOrder :: Ord t => t -> t -> (t,t,Bool)
inOrder a b = (x,y,z)
    where ~(x,y) | z = (a,b)
                 | otherwise = (b,a)
          z = a<=b

insertOrd :: Ord t => t -> [t] -> [t]
insertOrd e [] = [e]
insertOrd e (x:xs) = a:y:ys
  where (a,_,z) = inOrder e x
        ~(y:ys) = if z then x:xs else insertOrd e xs

{- | A range of shape (min,max) of ordered values.

Such ranges may be multiplied to create n-dimensional ranges for which
equivalence means sharing an n-dimensional subrange.  They may be very
useful in creating Maps that partition an n-dimensional space in which
we may query for subrange membership with logarithmic complexity for
any point P (a point is a subrange of volume 0, or `(pure x0,...,pure
xn) where (x0,..,xn) = p`).

Indeed, a point is equivalent to a range iff it belongs to that range.

-}
newtype Range a = Range (a,a)

instance Unit Range where pure a = Range (a,a)
-- | @r < r'@ iff all values of @r@ are below any value of @r'@
instance Ord a => Ord (Range a) where
  compare (Range (a,b)) (Range (a',b'))
    | b<a' = LT
    | b'<a = GT 
    | otherwise = EQ
-- | Range equivalence. Two ranges are equivalent iff they share a
-- common subrange (equivalence in this case is not transitive, so
-- beware of unintended consequences)
instance Ord a => Eq (Range a) where
  a == b = compare a b == EQ

extreme :: Bounded a => Bool -> a
extreme b = if b then maxBound else minBound

newtype Interleave a = Interleave { runInterleave :: [a] }
instance Semigroup (Interleave a) where
  Interleave ia + Interleave ib = Interleave (inter ia ib)
    where inter (a:as) bs = a:inter bs as
          inter [] bs = bs
deriving instance Monoid (Interleave a)

(&) :: a -> (a -> b) -> b
(&) = flip ($)
infixl 0 &
is :: a -> (a -> Bool) -> Bool
is = (&)

infixr 1 +++
(+++) :: Split k => (a -> k c c) -> (b -> k d d) -> (a:+:b) -> k (c,d) (c,d)
f +++ g = first.f <|> second.g

second :: Split k => k a b -> k (c,a) (c,b)
second a = id <#> a
first :: Split k => k a b -> k (a,c) (b,c)
first a = a <#> id

guard :: (Unit m,Monoid (m ())) => Bool -> m ()
guard p = if p then unit else zero

ifThenElse :: Bool -> a -> a -> a
ifThenElse b th el = if b then th else el
bool :: a -> a -> Bool -> a
bool th el b = ifThenElse b th el
tailSafe :: [a] -> [a]
tailSafe [] = [] ; tailSafe (_:t) = t
headDef :: a -> [a] -> a
headDef d [] = d ; headDef _ (x:_) = x

fail :: String -> a
fail = error
const :: Unit m => a -> m a
const = pure
fix :: (a -> a) -> a
fix f = y where y = f y

uncurry0 :: a -> () -> a
uncurry0 a _ = a
uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) = f a b
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c
uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

unit :: Unit m => m ()
unit = pure ()
when :: Unit m => Bool -> m () -> m ()
when p m = if p then m else unit
unless :: Unit m => Bool -> m () -> m ()
unless p m = if p then unit else m

invertOrd :: Ordering -> Ordering
invertOrd GT = LT ; invertOrd LT = GT ; invertOrd EQ = EQ

inside :: Ord t => t -> t -> (t -> Bool)
inside x y = \z -> x<=z && z<=y

rmod :: (RealFloat m,Invertible m) => m -> m -> m
a`rmod`b = b * r 
  where (_n,r) = c'couple c'int c'_ $ properFraction (a/b)
infixl 7 `rmod`

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

fromMaybe :: a -> Maybe a -> a
fromMaybe a = maybe a id

amb :: IO a -> IO a -> IO a
ma `amb` mb = do
  res <- newEmptyMVar
  ta <- forkIO $ P.handle (\(P.SomeException _) -> unit) $ ma P.>>= putMVar res . Left
  tb <- forkIO $ P.handle (\(P.SomeException _) -> unit) $ mb P.>>= putMVar res . Right

  takeMVar res P.>>= \c -> case c of
    Left a -> P.fmap (const a) (killThread tb)
    Right a -> P.fmap (const a) (killThread ta)
unamb :: a -> a -> a
unamb a b = unsafePerformIO (evaluate a `amb` evaluate b)

