{-# LANGUAGE MultiParamTypeClasses, ViewPatterns, ScopedTypeVariables, UndecidableInstances, DeriveGeneric #-}
module Data.Containers(
  -- * The basic data class
  DataMap(..),DataRel(..),Indexed(..),OrderedMap(..),Container(..),
  
  mat,lookup,mlookup,isKeyIn,member,delete,touch,insert,singleton,singleton',fromAList,fromKList,fromMAList,(#),(#?),(#?-),
  cached,keys,keysSet,

  -- * Map instances
  -- ** Sets and maps
  Set,Map,c'setOf,c'set,c'mapOf,c'map,c'bimapOf,c'bimap,c'Relation,
  
  -- ** Bimaps
  Bimap,toMap,

  -- ** Relations
  Relation,Equiv(..),i'domains,i'ranges,l'range,l'domain,l'rangeSet,l'domainSet,link,linkP,
  mapEdges,mapEdgesMonotonic,
  mapEquivEdges,mapEquivEdgesMonotonic,
  (*>>>),i'mapSet,i'boolMay
  )
  where

import Definitive.Base
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (Set)
import Control.Concurrent.MVar
import Control.DeepSeq
import GHC.Generics (Generic)

class Monoid m => DataMap m k a | m -> k a where
  at :: k -> Lens' m (Maybe a)
class Indexed f i | f -> i where
  keyed :: Iso (f (i,a)) (f (i,b)) (f a) (f b) 
class Container c where weight :: c a -> Int

instance Indexed [] Int where
  keyed = iso (zip [0..]) (map snd)
instance Container [] where weight = size
instance Container Set where weight = S.size
instance Container (Map k) where weight = M.size
class OrderedMap m k a | m -> k a where
  ascList :: Iso' m [(k,a)]
  nearest :: (Bool,Ordering) -> k -> m -> Maybe (k,a)
instance DataMap (f (Free f a)) k (Free f a) => DataMap (Free f a) [k] (Free f a) where
  at [] = lens Just f
    where f _ (Just x) = x
          f _ _ = zero
  at (x:xs) = at'.at xs
    where at' = lens f f'
            where f (Join m) = m^.at x.folded
                  f (Pure _) = zero
                  f' (Join m) v = Join (m & at x.folded %- v)
                  f' s _ = s
instance (Applicative f,DataMap (g a) k' a,DataMap (f (g a)) k (g a)) => DataMap ((f:.:g) a) (k,k') a where
  at (k,k') = from i'Compose.at k.l'Just zero.at k'

mat :: (DataMap m k a,Monoid a) => k -> Lens' m a
mat k = at k.folded

c'setOf :: Constraint a -> Constraint (Set a)
c'setOf _ = id
c'mapOf :: Constraint a -> Constraint b -> Constraint (Map a b)
c'mapOf _ _ = id
c'set :: Constraint (Set a)
c'set = c'setOf id
c'map :: Constraint (Map a b)
c'map = c'mapOf id id
c'bimapOf :: Constraint a -> Constraint b -> Constraint (Bimap a b)
c'bimapOf _ _ = id
c'bimap :: Constraint (Bimap a b)
c'bimap = c'bimapOf id id
c'Relation :: Constraint e -> Constraint a -> Constraint b -> Constraint (Relation e a b)
c'Relation _ _ _ = id

member :: DataMap m k Void => k -> Lens' m Bool
member k = at k.from i'maybe

lookup :: DataMap m k a => k -> m -> Maybe a
lookup s m = m^.at s
mlookup :: (DataMap m k a,Monoid a) => k -> m -> a
mlookup s m = m^.mat s

isKeyIn :: DataMap m k a => k -> m -> Bool
isKeyIn = map2 nonempty lookup
delete :: DataMap m k a => k -> m -> m
delete k = at k %- Nothing
insert :: DataMap m k a => k -> a -> m -> m
insert k a = at k %- Just a
(#) :: DataMap m k a => m -> [(k,a)] -> m
m # ks = compose [insert k a | (k,a) <- ks] m
touch :: (Monoid a, DataMap m k a) => k -> m -> m
touch k = mat k %~ id
singleton :: DataMap m k a => k -> a -> m
singleton = map2 ($zero) insert
singleton' :: (Monoid a,DataMap m k a) => k -> m
singleton' x = touch x zero
fromAList :: DataMap m k a => [(k,a)] -> m
fromAList l = compose (uncurry insert<$>l) zero
fromKList :: (Monoid a,DataMap m k a) => [k] -> m
fromKList l = compose (touch<$>l) zero
fromMAList :: (Monoid a,DataMap m k a) => [(k,a)] -> m
fromMAList l = compose [mat k %~ (+a) | (k,a) <- l] zero

instance Ord a => DataMap (Set a) a Void where
  at k = lens (S.member k) (flip (bool (S.insert k) (S.delete k))).i'maybe
instance Ord a => OrderedMap (Set a) a Void where
  ascList = iso S.toAscList S.fromAscList . mapping (i'_.commuted)
  nearest (True,LT) = map3 (,undefined) S.lookupLT
  nearest (True,GT) = map3 (,undefined) S.lookupGT
  nearest (False,LT) = map3 (,undefined) S.lookupGE
  nearest (False,GT) = map3 (,undefined) S.lookupLE
  nearest (True,EQ) = \k s -> (k,undefined) <$ guard (S.member k s)
  nearest (False,EQ) = \k s -> S.minView (S.delete k s) <&> second (const undefined)
instance Ord k => DataMap (Map k a) k a where
  at k = lens (M.lookup k) (\m a -> M.alter (const a) k m)
instance Eq k => DataMap [(k,a)] k a where
  at k = lens (foldMap (\(k',a) -> a <$ guard (k==k'))) g
    where g l Nothing = [(k',a) | (k',a) <- l, k' /= k ]
          g l (Just a) = (k,a) : l
instance Ord k => OrderedMap (Map k a) k a where 
  ascList = iso M.toAscList M.fromAscList
  nearest (True,LT) = M.lookupLT
  nearest (True,GT) = M.lookupGT
  nearest (False,LT) = M.lookupGE
  nearest (False,GT) = M.lookupLE
  nearest (True,EQ) = \k m -> M.lookup k m <&> (k,)
  nearest (False,EQ) = \k m -> M.minViewWithKey (M.delete k m) <&> fst
  
instance Ord a => Semigroup (Set a) where (+) = S.union
instance Ord a => Monoid (Set a) where zero = S.empty
instance Ord a => Disjonctive (Set a) where (-) = S.difference
instance Ord a => Semiring (Set a) where (*) = S.intersection
instance Functor Set where map = S.mapMonotonic
instance Foldable Set where fold = S.foldr (+) zero
instance Ord b => Compound a b (Set a) (Set b) where
  each k s = fromKList<$>traverse k (toList s)

instance Ord k => Semigroup (Map k a) where (+) = M.union
instance Ord k => Monoid (Map k a) where zero = M.empty
instance Ord k => Disjonctive (Map k a) where (-) = M.difference
instance (Ord k,Semigroup a) => Semiring (Map k a) where (*) = M.unionWith (+)
instance Functor (Map k) where map = M.map
instance Foldable (Map k) where fold = M.foldr (+) zero
instance Ord k => Traversable (Map k) where sequence m = sequence [(k,)<$>fa | (k,fa) <- m^.ascList] <&> yb ascList
instance Indexed (Map k) k where keyed = iso (M.mapWithKey (,)) (map snd)

instance Ord k => SemiApplicative (Zip (Map k)) where
  Zip fs <*> Zip xs = Zip (M.intersectionWith ($) fs xs)

-- |An invertible map
newtype Bimap a b = Bimap (Map a b,Map b a)
                  deriving (Show,Semigroup,Monoid,Disjonctive,Semiring,Generic,NFData)
instance Commutative Bimap where
  commute (Bimap (b,a)) = Bimap (a,b)

instance (Ord a,Ord b) => DataMap (Bimap a b) a b where
  at a = lens t'lookup setAt
    where t'lookup ma = toMap ma^.at a
          setAt (Bimap (ma,mb)) b' = Bimap (
            maybe id delete (b' >>= \b'' -> mb^.at b'') ma & at a %- b',
            mb & maybe id delete b >>> maybe id (flip insert a) b')
            where b = ma^.at a 
instance (Ord b,Ord a) => DataMap (Flip Bimap b a) b a where
  at k = from (commuted.i'Flip).at k
instance (Ord a,Ord b) => OrderedMap (Bimap a b) a b where
  ascList = iso (toMap >>> \m -> m^.ascList) (\l -> Bimap (l^..ascList,fromAList (map swap l)))
  nearest x k m = nearest x k (toMap m)
toMap :: Bimap a b -> Map a b
toMap (Bimap (a,_)) = a

keys :: (OrderedMap m k a) => m -> [k]
keys m = map fst (m^.ascList)
keysSet :: (Ord k,OrderedMap m k a) => m -> Set k
keysSet m = map (second (const zero)) (m^.ascList)^.from ascList

--- |The Relation type
newtype Relation e a b = Relation (Map a (Map b e),Map b (Map a e))
                     deriving (Show,Eq,Ord,Generic,NFData)
instance (Ord a,Ord b) => Semigroup (Relation e a b) where
  Relation (x,x') + Relation (y,y') = Relation (M.unionWith (+) x y,M.unionWith (+) x' y')
deriving instance (Ord a,Ord b) => Monoid (Relation e a b)
instance (Semigroup e,Ord a,Ord b) => Semiring (Relation e a b) where
  Relation (x,x') * Relation (y,y') = Relation (zipWith (zipWith (+)) x y,zipWith (zipWith (+)) x' y')
instance Commutative (Relation e) where
  commute (Relation (a,b)) = Relation (b,a)

-- |Define a Relation from its domains. 
i'ranges :: (Ord c,Ord d) => Iso (Map a (Map b e)) (Map c (Map d e')) (Relation e a b) (Relation e' c d)
i'ranges = iso (\(Relation (rs,_)) -> rs) fromDomains
  where fromDomains rs = Relation (rs,compose (rs^.keyed <&> \ (a,bs) -> compose $ bs^.keyed <&> \(b,e) ->
                                                 at b %~ Just . insert a e . fold) zero)
-- |Define a Relation from its ranges. O(1) <-> O(1,n*ln(n)) 
i'domains :: (Ord c,Ord d) => Iso (Map b (Map a e)) (Map d (Map c e')) (Relation e a b) (Relation e' c d)
i'domains = iso commute commute.i'ranges

i'mapSet :: (Ord a,Ord b,Monoid n) => Iso (Set a) (Set b) (Map a m) (Map b n)
i'mapSet = iso keysSet (\s -> fromAList [(a,zero) | a <- toList s]) 
i'boolMay :: Monoid m => Iso' Bool (Maybe m)
i'boolMay = iso (\b -> if b then Just zero else Nothing) (maybe False (const True))

instance (Ord a,Ord b) => DataMap (Relation e a b) a (Map b e) where
  at a = lens (\(Relation (rs,_)) -> rs^.at a) setRan
    where setRan (Relation (rs,ds)) ran0 = Relation (
            rs & at a %- if empty ran then Nothing else ran0,
            adjust ds)
            where ran = toRan $ ran0 ; oldRan = toRan $ rs^.at a
                  toRan m = maybe zero id m^.keyed
                  adjust = compose ((oldRan-ran) <&> \(b,_) -> at b.traverse.at a %- Nothing)
                           >>> compose (ran <&> \(b,e) -> at b.folded %~ insert a e)

may :: (Monoid (f b),Foldable f) => Iso (Maybe (f a)) (Maybe (f b)) (f a) (f b)
may = iso (\f -> if empty f then Nothing else Just f) (maybe zero id)

class (DataMap r a (Map b e),DataMap r' b (Map a e)) => DataRel r r' e a b | r -> r' e a b where
  i'inverseRel :: Iso' r r'
  
instance (Ord a,Ord b) => DataRel (Relation e a b) (Relation e b a) e a b where
  i'inverseRel = commuted
l'range :: (DataRel r r' e a b,Ord b) => a -> Lens' r (Map b e)
l'range a = at a.from may
l'rangeSet :: (DataRel r r' e a b,Ord b,Monoid e) => a -> Lens' r (Set b)
l'rangeSet a = l'range a.i'mapSet
l'domain :: (DataRel r r' e a b,Ord a) => b -> Lens' r (Map a e)
l'domain a = i'inverseRel.at a.from may
l'domainSet :: (DataRel r r' e a b,Ord a,Monoid e) => b -> Lens' r (Set a)
l'domainSet a = i'inverseRel.at a.from may.i'mapSet

link :: (DataRel r r' e a b,Ord b) => a -> b -> Lens' r (Maybe e)
link a b = l'range a.at b
linkP :: (DataRel r r' e a b,Ord b,Monoid e) => a -> b -> Lens' r Bool
linkP a b = link a b.from i'boolMay

newtype Equiv e a = Equiv (Relation e a a)
                  deriving (Generic,NFData)
instance (Show e,Monoid e,Ord a,Show a) => Show (Equiv e a) where
  show e@(Equiv r) = "Equiv "+show (keys (r^.i'domains) <&> \a -> e^.l'range a)
instance (Monoid e,Ord a) => Semigroup (Equiv e a) where
  ra + Equiv rb = let repr m = foldr const zero m
                  in foldr (\(a,as) -> warp (l'domain a) (\bs -> map (+repr bs) as+map (repr as+) bs))
                     ra (rb^.i'domains.ascList)
deriving instance (Monoid e,Ord a) => Monoid (Equiv e a)
instance (Ord a,Monoid e) => DataMap (Equiv e a) a (Map a e) where
  at a = lens (Just<$>getR) setR
    where setR (Equiv r) as = Equiv (r
                                    & ((\(c:cs) -> unifyCanonical c cs) $ ascList
                                       $^ fold [r^.l'range a' & map (+e') . touch a'
                                               | (a',e') <- (a,e):added])
                                    & compose [(l'range d %- zero) >>>
                                               \r' -> let rng = r'^.l'domain d in
                                               if empty rng then r'
                                               else r' & l'domain d %- zero
                                                    & l'domain (fst (head (ascList$^rng))) %- rng
                                              | d <- fst<$>deleted])
            where as'' = fromMaybe zero as ; as' = getR (Equiv r)
                  deleted = ascList$^(as' - as'') ; added = ascList$^(as'' - as')
                  e = fold as''
          getR (Equiv r) = foldMap (\c -> r^.l'domain c) $ keys (r^.l'range a)
instance (Ord a,Monoid e) => DataRel (Equiv e a) (Equiv e a) e a a where
  i'inverseRel = id

unifyCanonical :: (Monoid e,DataRel r r' e a a,Ord a) => (a,e) -> [(a,e)] -> r -> r
unifyCanonical (c,e) cs rel = fst $ foldr unify' (rel & l'domain c %~ map (+e) . touch c,e) cs
  where unify' (c',e'') (r,e') = (r & l'domain c' %- zero & l'domain c %~ merge,e+e'')
          where m' = r^.l'domain c'
                merge m = (e'+e'') <$ touch c' (m + m')

type MapRelationEdges a b e e' = (a -> Maybe a) -> (b -> Maybe b) -> (e -> Maybe e') -> Relation e a b -> Relation e' a b

mapEdges :: (Ord a, Ord b) => MapRelationEdges a b e e'
mapEdges fa fb fe (Relation (x,y)) = Relation (filt fa fb x,filt fb fa y)
  where filt fa' fb' m = fromAList [(a,filt' m') | (Just a,m') <- map (first fa') (m^.ascList)]
          where filt' m' = fromAList [(b,e) | (Just b,Just e) <- map (fb'<#>fe) (m'^.ascList)]
mapEdgesMonotonic :: (Ord a, Ord b,Eq e,Eq e') => MapRelationEdges a b e e'
mapEdgesMonotonic fa fb fe (Relation (x,y)) = Relation (filt fa fb x,filt fb fa y)
  where filt fa' fb' m = yb ascList [(a,filt' m') | (Just a,m') <- map (first fa') (m^.ascList)]
          where filt' m' = yb ascList [(b,e) | (Just b,Just e) <- map (fb'<#>fe) (m'^.ascList)]

_mapEquivEdges :: Ord a => MapRelationEdges a a e e' -> (a -> Maybe a) -> (e -> Maybe e') -> Equiv e a -> Equiv e' a
_mapEquivEdges _map fa fe (Equiv r) =
  let r' = _map fa Just fe r
      adjCan l = fold [
        case (guard (isKeyIn a m) >> pure a) + fa a + map fst (nearest (False,EQ) a m) of
          Just a' -> [(a',m)]
          Nothing -> []
        | (a,m) <- l]
  in Equiv $ warp (i'domains.iso (by ascList) fromAList) adjCan r'
 
mapEquivEdges :: Ord a => (a -> Maybe a) -> (e -> Maybe e') -> Equiv e a -> Equiv e' a
mapEquivEdges = _mapEquivEdges mapEdges
mapEquivEdgesMonotonic :: (Ord a, Eq e, Eq e') => (a -> Maybe a) -> (e -> Maybe e') -> Equiv e a -> Equiv e' a
mapEquivEdgesMonotonic = _mapEquivEdges mapEdgesMonotonic


(#?) :: (Ord a,Ord b) => Relation e a b -> [(a,e,b)] -> Relation e a b
r #? ls = compose [link a b %- Just e | (a,e,b) <- ls] r
(#?-) :: (Ord a,Ord b,Monoid e) => Relation e a b -> [(a,b)] -> Relation e a b
r #?- ls = compose [link a b %- Just zero | (a,b) <- ls] r

cached :: forall a b. Ord a => (a -> b) -> a -> b
cached f = \a -> g a^.thunk
  where g a = do
          m <- vm `seq` takeMVar vm
          case m^.at a of
            Just b -> putMVar vm m >> return b
            Nothing -> let b = f a in putMVar vm (insert a b m) >> return b
        vm = newMVar (zero :: Map a b)^.thunk

(*>>>) :: (Ord a,Ord b,Ord c) => Relation e a b -> Relation e b c -> Relation e a c
r *>>> r' = r & i'ranges %~ map (\m -> m^.keyed & foldMap (\(b,_) -> r'^.l'range b))
