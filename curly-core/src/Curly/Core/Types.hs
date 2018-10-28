{-# LANGUAGE CPP, RecursiveDo, ViewPatterns, PatternSynonyms, ScopedTypeVariables, UndecidableInstances #-}
module Curly.Core.Types (
  -- * Type constructors
  TypeClass(..),NativeType(..),TypeShape(..),
  -- * Type paths
  TypeIndex(..),pattern In,pattern Out,PathRoot(..),TypePath,
  t'ImplicitRoot,t'ContextRoot,
  -- * Types
  Type(..),
  argumentType,builtinType,rigidTypeFun,
  typeConstraints,
  extractFirstArgument,
  mapTypePaths,mapTypePathsMonotonic,traverseTypeShapes,mapTypeShapes,
  selectConstraints,clearContexts,abstractStructTypes,abstractImplicitType,
  functionFrom,
  freezeType,thawType,isComplexType,compareConstrainedness,isSubtypeOf,constraintType,
  -- * Implicit instances
  InstanceMap,isValidInstanceMap
  )
       where

import Definitive
import Curly.Core
import Curly.Core.Documentation
import Language.Format
import qualified Data.Map
import Control.DeepSeq

i'equivRel :: Iso (Relation e a a) (Relation e' a' a') (Equiv e a) (Equiv e' a')
i'equivRel = iso (\(Equiv r) -> r) Equiv
t'assoc :: (OrderedMap m k a,OrderedMap m' k' a') => Traversal (k,a) (k',a') m m'
t'assoc = i'ascList.traverse
i'ascList :: (OrderedMap m k a,OrderedMap m' k' a') => Iso [(k,a)] [(k',a')] m m'
i'ascList = iso (by ascList) (yb ascList)

data TypeClass s = Function | NamedType Int s | ClassType Int [Set Int] s
               deriving (Eq,Ord,Generic)
instance Identifier s => Show (TypeClass s) where
  show Function = "->"
  show (NamedType _ s) = identName s
  show (ClassType _ _ s) = identName s
instance HasIdents s s' (TypeClass s) (TypeClass s') where
  ff'idents _ Function = pure Function
  ff'idents k (NamedType n s) = NamedType n<$>k s
  ff'idents k (ClassType n is s) = ClassType n is<$>k s
instance NFData s => NFData (TypeClass s)
instance Serializable Bytes s => Serializable Bytes (TypeClass s)
instance Format Bytes s => Format Bytes (TypeClass s)

typeClassNArgs :: TypeClass s -> Int
typeClassNArgs Function = 2
typeClassNArgs (NamedType n _) = n
typeClassNArgs (ClassType n _ _) = n

data NativeType = NT_RigidType String | NT_Int | NT_String | NT_Array | NT_Unit | NT_File | NT_Syntax | NT_Expr 
                deriving (Eq,Ord,Generic)
instance Show NativeType where
  show (NT_RigidType t) = "#."+t
  show NT_Int = "#int" ; show NT_String = "#string"
  show NT_Unit = "#unit" ; show NT_File = "#file"
  show NT_Syntax = "#syn" ; show NT_Expr = "#expr"
  show NT_Array = "#array"
instance Serializable Bytes NativeType
instance Format Bytes NativeType
instance NFData NativeType

-- | An index into a type
data TypeIndex s = TypeIndex (TypeClass s) Int
                 deriving (Eq,Ord,Generic)
instance Identifier s => Show (TypeIndex s) where
  show (TypeIndex c n) = show c+":"+show n
instance HasIdents s s' (TypeIndex s) (TypeIndex s') where
  ff'idents k (TypeIndex c i) = forl ff'idents c k <&> \c' -> TypeIndex c' i
instance Serializable Bytes s => Serializable Bytes (TypeIndex s)
instance Format Bytes s => Format Bytes (TypeIndex s)
instance NFData s => NFData (TypeIndex s)
pattern In :: TypeIndex t
pattern In = TypeIndex Function 0
pattern Out :: TypeIndex t
pattern Out = TypeIndex Function 1

{- | The path of a node inside a type.

A path is comprised of two parts : a canonical path, which uniquely identifies the node within
its type graph; and a set of equivalent paths that are shared between all types.
-}
data PathRoot = ArgumentRoot Int | TypeRoot | ImplicitRoot Int | ContextRoot Int | NamedRoot String
              deriving (Eq,Ord,Show,Generic)
t'ImplicitRoot :: Traversal' PathRoot Int
t'ImplicitRoot k (ImplicitRoot n) = ImplicitRoot<$>k n
t'ImplicitRoot _ x = pure x
t'ContextRoot :: Traversal' PathRoot Int
t'ContextRoot k (ContextRoot n) = ContextRoot<$>k n
t'ContextRoot _ x = pure x
instance Serializable Bytes PathRoot
instance Format Bytes PathRoot
instance NFData PathRoot
type TypePath s = (PathRoot,[TypeIndex s])
pathIdents :: FixFold s s' (TypePath s) (TypePath s')
pathIdents = l'2.each.ff'idents

-- | The shape of a Curly type
data TypeShape s = TypeCons (TypeClass s)
                 | NativeType NativeType
                 | PolyType
                 | SkolemType Int
                 | TypeMismatch (TypeShape s) (TypeShape s)
                 | HiddenTypeError
                 deriving (Show,Eq,Ord,Generic)
instance Ord s => Semigroup (TypeShape s) where
  TypeCons c + TypeCons c' | c == c' = TypeCons c
  NativeType t + NativeType t' | t == t' = NativeType t
  SkolemType x + SkolemType x' | x == x' = SkolemType x
  PolyType + t = t
  t + PolyType = t
  t + t' = TypeMismatch t t'
instance Ord s => Monoid (TypeShape s) where zero = PolyType
instance Ord s' => HasIdents s s' (TypeShape s) (TypeShape s') where
  ff'idents k (TypeCons c) = map TypeCons (forl ff'idents c k)
  ff'idents k (TypeMismatch t t') = liftA2 TypeMismatch (forl ff'idents t k) (forl ff'idents t' k)
  ff'idents _ (NativeType n) = pure (NativeType n)
  ff'idents _ PolyType = pure PolyType
  ff'idents _ (SkolemType x) = pure (SkolemType x)
  ff'idents _ HiddenTypeError = pure HiddenTypeError
instance Serializable Bytes s => Serializable Bytes (TypeShape s) where
  encode p (TypeCons Function) = encodeAlt p 0 ()
  encode p (TypeCons (NamedType n s)) = encodeAlt p 1 (n,s)
  encode p (TypeCons (ClassType n is s)) = encodeAlt p 2 (n,is,s)
  encode p (NativeType t) = encodeAlt p 3 t
  encode p PolyType = encodeAlt p 4 ()
  encode p (SkolemType x) = encodeAlt p 5 x
  encode p (TypeMismatch t t') = encodeAlt p 6 (t,t')
  encode p HiddenTypeError = encodeAlt p 7 ()
instance (Format Bytes s,Ord s) => Format Bytes (TypeShape s) where
  datum = datumOf [FormatAlt (uncurry0 $ TypeCons Function)
                  ,FormatAlt (\(n,s) -> TypeCons (NamedType n s))
                  ,FormatAlt (\(n,is,s) -> TypeCons (ClassType n is s))
                  ,FormatAlt NativeType
                  ,FormatAlt (uncurry0 PolyType)
                  ,FormatAlt SkolemType
                  ,FormatAlt (uncurry TypeMismatch)
                  ,FormatAlt (uncurry0 HiddenTypeError)]
instance NFData s => NFData (TypeShape s)

{- | A Curly type.

A Curly type may be understood as a (possibly infinite) set of
constraints over its graph ("the node A must be a function", "the
output of B must be an Int", ...).

In that sense, Curly types are monoids by isomorphism with the set
monoid (@C(a+b)=C(a)+C(b)@ where @C(a)@ is the constraint set of the
type @a@). This monoid instance is used to perform type inference by
unifying constraints on the appropriate types.

-}
newtype Type s = Type (Equiv (TypeShape s) (TypePath s))
               deriving Generic
instance (Ord s,Serializable Bytes s) => Serializable Bytes (Type s)
instance (Ord s,Format Bytes s) => Format Bytes (Type s)
instance NFData s => NFData (Type s)
type TypeRel s = Equiv (TypeShape s) (TypePath s)
i'typeRel :: Iso (TypeRel s) (TypeRel s') (Type s) (Type s')
i'typeRel = iso (\(Type s) -> s) Type
instance Identifier s => Semigroup (Type s) where
  t + t' = getId (zipTypes (\_ _ -> pure ()) t t')
instance Identifier s => Monoid (Type s) where zero = Type zero
instance (Ord s,Ord s') => HasIdents s s' (Type s) (Type s') where
  ff'idents k (Type e) = map Type
                         $ forl (i'equivRel.i'ranges
                                 .t'assoc.(
                                   l'1.pathIdents
                                   .+l'2.t'assoc.(l'1.pathIdents
                                                  .+l'2.ff'idents))) e k
instance Identifier s => Eq (Type s) where t == t' = compare t t' == EQ
instance Identifier s => Ord (Type s) where
  compare t t' = case zipTypes cmpNodes t t' of
    Left x -> x
    Right _ -> EQ
    where cmpNodes PolyType _ = unit
          cmpNodes _ PolyType = unit
          cmpNodes s s' = case compare s s' of
            EQ -> unit
            x -> Left x

compareConstrainedness :: Identifier s => Type s -> Type s -> Maybe Ordering
compareConstrainedness t t' = case fst (zipTypes cmp t t') of
    (True,False) -> Just GT
    (False,True) -> Just LT
    (False,False) -> Just EQ
    (True,True) -> Nothing
    where cmp x y | x==y = unit
          cmp PolyType _ = tell (False,True)
          cmp _ PolyType = tell (True,False)
          cmp _ _ = tell (True,True)
isSubtypeOf :: Identifier s => Type s -> Type s -> Bool
isSubtypeOf t t' = compareConstrainedness t t' `elem` [Just GT,Just EQ]

instance Identifier s => Show (Type s) where
  show (Type e) = (arguments+implicits+body+context+subs)
    where paths :: Map (Set (TypePath s)) (Maybe (TypeClass s),Maybe String,String)
          ~(paths,_,_) = execS (traverseTypeShapes f (Type e)^..state) (c'map zero,tail (namesFrom ['a'..'z']),tail (namesFrom ['A'..'Z']))
            where f ps x = x <$ do
                    let p:_ = ps
                    modify $ \(m,as,bs) ->
                      let ins a b c = insert (fromKList ps) (a,b,c) m
                      in if isKeyIn (fromKList ps) m then (m,as,bs) else case x of
                      TypeCons c -> let short = guard (length ps>1) >> pure (head bs)
                                    in (ins (Just c) short (showPat (showC c) [let p' = second (+[TypeIndex c i]) p
                                                                               in case lookup (e^.l'rangeSet p') paths of
                                                                                 Just (pri,ms,s) -> fromMaybe ((if hasPrecedence i c pri then format "(%s)" else id) s) ms
                                                                                 Nothing -> "#<path:"+showPath p'+">"
                                                                              | i <- [0..typeClassNArgs c-1]]),
                                        as,maybe id (pure tail) short bs)
                      PolyType | length ps>1 -> (ins Nothing Nothing (head as),tail as,bs)
                               | otherwise -> (ins Nothing Nothing "*",as,bs)
                      NativeType t -> (ins Nothing Nothing (showNT t),as,bs)
                      SkolemType _ | length ps>1 -> (ins Nothing Nothing (head bs),as,tail bs)
                                   | otherwise -> (ins Nothing Nothing ".",as,bs)
                      _ -> (ins Nothing Nothing "∅",as,bs)
          hasPrecedence 0 Function (Just Function) = True
          hasPrecedence _ _ (Just c) | typeClassNArgs c==0 = False
          hasPrecedence _ _ Nothing = False
          hasPrecedence _ x (Just y) = x > y
          showPat "_" as = intercalate " " as
          showPat ('_':p) (a:as) = format "%s%s" a (showPat p as)
          showPat (c:p) as = c:showPat p as
          showPat [] as = intercalate " " ("":as)
          showC Function = "_ -> _"
          showC (NamedType _ s) = identName s
          showC (ClassType _ _ s) = identName s
          showNT NT_Int = "#int"
          showNT NT_String = "#string"
          showNT NT_Unit = "#unit"
          showNT NT_File = "#file"
          showNT NT_Syntax = "#syn"
          showNT NT_Expr = "#expr"
          showNT NT_Array = "#array"
          showNT (NT_RigidType t) = t
          showPath (r,p) = show r+":"+foldMap showPComp p
            where showPComp In = "I"
                  showPComp Out = "O"
                  showPComp (TypeIndex c n) = format "(%s,%s)" (show c) (show n)
          pathList = paths^.ascList <&> \(x,(_,y,z)) -> (x,(y,z))
          namesFrom cs = "":fold (deZip (for cs (\c -> Zip (map (c:) (namesFrom cs)))))
          arguments = let x = intercalate "," [format "(%d)%s" i (snd s) | (ps,s) <- pathList, (ArgumentRoot i,[]) <- keys ps]
                      in if empty x then "" else format "[%s] " x
          implicits = let x = intercalate "," [snd s | (ps,s) <- pathList, (ImplicitRoot _,[]) <- keys ps]
                      in if empty x then "" else x+" => "
          body = fold [fromMaybe (snd s) (fst s) | (ps,s) <- pathList, (TypeRoot,[]) <- keys ps]
          context = let x = intercalate "," [snd s | (ps,s) <- pathList, (ContextRoot _,[]) <- keys ps]
                    in if empty x then "" else " <= "+x
          subs = interleave ("\n  where ":repeat "\n        ") $ "":[
            format "%s = %s" short long
            | (_,(Just short,long)) <- pathList]
instance Identifier s => Documented (Type s) where
  document t = docTag' "type" [Pure (show t)]

zipTypes :: (Monad m,Identifier s) => (TypeShape s -> TypeShape s -> m ()) -> Type s -> Type s -> m (Type s)
zipTypes zipNodes (Type ta) (Type tb) =
  map Type
  $ traverseEquivEdges (\_ (shl,shr) -> mergeSkol shl shr <$ zipNodes shl shr)
  $ saturate (ta' + tagEdges (zero,) tb)
  where tagEdges f = mapEquivEdgesMonotonic Just (Just . f)
        (Max nska,ta') = tell (Max (-1)) >> traverseEquivEdges go ta
          where go _ x@(SkolemType n) = tell (Max n) >> pure (x,zero)
                go _ x = pure (x,zero)

        mergeSkol (SkolemType n) (SkolemType _) = SkolemType n
        mergeSkol PolyType (SkolemType n) = SkolemType (n+nska+1)
        mergeSkol a b = a+b

        nextPath c i = second (+[TypeIndex c i])
        isPathPrefix (r,p) (r',p') = r==r' && let (ph',pt') = splitAt (length p) p' in ph'==p && nonempty pt'
        withoutPrefixes l = [p' | (p,p') <- zip (head l:l) l
                                , not (isPathPrefix p p')]

        unifyNodes nodes@(can:_) = l'range can %~ (+fromKList nodes)
        unifyNodes _ = id

        pathSet = c'set . yb ascList . map (,undefined)
        saturate rel =
          case [map (nextPath c i . fst) domL+nextDom
               | (can,dom) <- rel^.i'equivRel.i'domains.ascList
               , let domL = dom^.ascList
               , TypeCons c <- take 1 $ map (uncurry (+) . snd) domL
               , i <- [0..typeClassNArgs c-1]
               , let projDom = map (nextPath c i) (withoutPrefixes $ map fst domL)
                     nextDomL = rel^.l'domain (nextPath c i can).ascList
                     nextDom = map fst nextDomL
               , nonempty (pathSet projDom - pathSet nextDom)]
          of paths : _ -> saturate (unifyNodes paths rel)
             [] -> rel

type TypeSkel s = Free ((,) (TypeClass s):.:[]) (TypeShape s)

poly :: TypeSkel s
(-->) :: TypeSkel s -> TypeSkel s -> TypeSkel s
poly = Pure PolyType
infixr 3 -->
a --> b = Join (Compose (Function,[a,b]))

lnSkel :: Ord s => TypePath s -> TypePath s -> TypeSkel s -> Equiv (TypeShape s) (TypePath s) -> Equiv (TypeShape s) (TypePath s)
lnSkel p p' = \x -> case x of
  Join (Compose (c,as)) -> set (link p p') (Just (TypeCons c))
                           . compose (zipWith (\i a -> let down = (+[TypeIndex c i])
                                                       in lnSkel (second down p) (second down p') a) [0..] as)
  Pure sh -> warp (link p p') (Just . (sh+) . fold)

ln :: Ord s => [TypeIndex s] -> [TypeIndex s] -> TypeSkel s -> Equiv (TypeShape s) (TypePath s) -> Equiv (TypeShape s) (TypePath s)
ln p p' = lnSkel (TypeRoot,p) (TypeRoot,p')

ln' :: Ord s => [TypeIndex s] -> TypeSkel s -> Equiv (TypeShape s) (TypePath s) -> Equiv (TypeShape s) (TypePath s)
ln' p = ln p p

mapTypePaths :: Ord s => (TypePath s -> Maybe (TypePath s)) -> Type s -> Type s
mapTypePaths f = i'typeRel %~ mapEquivEdges f Just
mapTypePathsMonotonic :: Ord s => (TypePath s -> Maybe (TypePath s)) -> Type s -> Type s
mapTypePathsMonotonic f = i'typeRel %~ mapEquivEdgesMonotonic f Just
traverseEquivEdges :: (Ord a,Ord e,Ord e',Monad m) => ([a] -> e -> m e') -> Equiv e a -> m (Equiv e' a)
traverseEquivEdges f = traversel (i'equivRel.i'domains.i'ascList) $ \l ->
  sequence [traversel (l'2.i'ascList.each.l'2) (f (keys m)) x
           | x@(_,m) <- l]
traverseTypeShapes :: (Ord s,Monad m) => ([TypePath s] -> TypeShape s -> m (TypeShape s)) -> Type s -> m (Type s)
traverseTypeShapes f = traversel (i'typeRel.i'equivRel.i'domains.ascList) $ \l ->
  sequence [traversel (l'2.i'ascList.each.l'2) (f (keys m)) x
           | x@(_,m) <- l]
mapTypeShapes :: Ord s => ([TypePath s] -> TypeShape s -> TypeShape s) -> Type s -> Type s
mapTypeShapes f = getId . traverseTypeShapes (map2 Id f)

selectConstraints :: Ord s => (Int -> Maybe Int) -> (Int -> Maybe Int) -> Type s -> Type s
selectConstraints ip cp = mapTypePathsMonotonic f
  where f (ContextRoot n,p) = map (\n' -> (ContextRoot n',p)) (cp n)
        f (ImplicitRoot n,p) = map (\n' -> (ImplicitRoot n',p)) (ip n)
        f x = Just x

extractFirstArgument :: Identifier s => Type s -> Type s
extractFirstArgument = warp i'typeRel addCons . mapTypePaths (Just . f)
  where f (TypeRoot,p) = (TypeRoot,Out:p)
        f (ArgumentRoot 0,p) = (TypeRoot,In:p)
        f (ArgumentRoot n,p) = (ArgumentRoot (n-1),p)
        f x = x
        addCons = ln' [] (poly --> poly)
clearContexts :: Ord s => Type s -> Type s
clearContexts = selectConstraints Just (const Nothing)

isComplexType :: Ord s => Type s -> Bool
isComplexType = fst . traverseTypeShapes isC
  where isC ps x = (isCS ps x,x)
        isCS ps (TypeCons _) = any (\p -> any (isPathPrefix p) ps) ps
        isCS _ _ = False
        isPathPrefix (r,p) (r',p') = r==r' && isPrefix p p' && p/=p'
        isPrefix [] _ = True
        isPrefix (x:l) (x':l') | x==x' = isPrefix l l'
        isPrefix _ _ = False

typeConstraints :: Ord s => Type s -> ([(s,[Set Int])],[(s,[Set Int])])
typeConstraints t = let l = t^.i'typeRel.i'equivRel.i'domains.ascList.mapping (mapping ascList)
                    in ([(s,is) | ((ImplicitRoot _,[]),[(_,TypeCons (ClassType _ is s))]) <- l]
                       ,[(s,is) | ((ContextRoot _,[]),[(_,TypeCons (ClassType _ is s))]) <- l])


argumentType :: Ord s => Int -> Type s
argumentType n = Type (set (linkP (TypeRoot,[]) (ArgumentRoot n,[])) True zero)
rigidTypeFun :: Ord s => String -> Type s
rigidTypeFun n = zero
                 & ln' [] (poly --> poly)
                 . lnn [In] [] n
                 . lnn [Out] [] n
                 & Type
  where lnn p p' = lnSkel (TypeRoot,p) (NamedRoot n,p') . Pure . NativeType . NT_RigidType
        
-- | Create a pair of constructor/destructor types
abstractStructTypes :: Ord s => s -> [String] -> [String] -> Type s -> (Type s,Type s)
abstractStructTypes c pub priv (Type e) = (Type e,Type e) & mk True <#> mk False
  where (classPos,_) = (traverseTypeShapes k (Type e)^..state) (c'map zero)
          where k ps sh@(NativeType (NT_RigidType s))
                  | isPrivate s = do
                      for_ ps $ \(p,_) -> mat p.l'1 =- True
                      return sh
                k ps PolyType = do
                  for_ ps $ \(p,_) -> mat p.l'2 =- True
                  return PolyType
                k _ x = return x
        tc = NamedType (length pub) c
        (il,cl) = partition snd [(n,x) | (ImplicitRoot n,(x,y)) <- classPos^.ascList
                                       , x/=y]
        (toI,toC) = (to il,to cl)
          where to l = \i -> fromMaybe i $ lookup i m
                  where m = c'map $ zipWith (\i (j,_) -> (j,i)) [0..] l^..ascList
        privSet = c'set (fromKList priv)
        isPrivate s = s`isKeyIn`privSet
        isExtended = nonempty il || nonempty cl
        lntc pref = ln' pref (Pure $ TypeCons tc)
                    . compose (zipWith (\i n -> lnSkel (TypeRoot,pref+[TypeIndex tc i]) (NamedRoot n,[]) poly)
                               [0..] pub)
        delNamedRoots = mapTypePaths f'
          where f' (NamedRoot _,_) = Nothing
                f' x = Just x
        runIdGen ma = evalS (ma^..state) (0,c'map zero)
        nextID ps = id <~ \(n,m) -> case lookup ps m of
          Just i -> ((n,m),i)
          Nothing -> ((n+1,insert ps n m),n)
        mk isCons t =
          delNamedRoots $
          if isCons
          then let f (TypeRoot,p) = Just (TypeRoot,In:p)
                   f (r@(ImplicitRoot n),p) = lookup r classPos >>= \(isP,isF) -> do
                     guard (isP/=isF)
                     return (if isP then ImplicitRoot (toI n) else ContextRoot (toC n),p)
                   f (ContextRoot _,_) = Nothing
                   f x = Just x
                   g ps PolyType = SkolemType <$> nextID ps
                   g _ (NativeType (NT_RigidType _)) = pure PolyType
                   g _ x = pure x
                   addCons = ln' [] (poly --> poly) . lntc [Out]
               in warp i'typeRel addCons $ mapTypePaths f $ runIdGen $ traverseTypeShapes g t
          else let f (TypeRoot,p) = Just (TypeRoot,(if isExtended then ([In,In]+) else (Out:)) p)
                   f (r@(ImplicitRoot n),p) = lookup r classPos >>= \(isP,isF) -> do
                     guard (isP/=isF)
                     return (if isP then ContextRoot (toI n) else ImplicitRoot (toC n),p)
                   f (ContextRoot _,_) = Nothing
                   f x = Just x
                   g ps (NativeType (NT_RigidType s)) | isPrivate s = SkolemType <$> nextID ps
                                                      | otherwise = pure PolyType
                   g _ x = pure x
                   strPref = if isExtended then [Out,In] else [In]
                   addCons = lntc strPref
                             . if isExtended
                               then ln' [] ((poly --> poly) --> (poly --> poly))
                                    . ln [In,Out] [Out,Out] poly
                               else ln' [] (Pure (TypeCons Function))
               in runIdGen $ traverseTypeShapes g $ warp i'typeRel addCons $ mapTypePaths f $ t

-- | 
abstractImplicitType :: Ord s => (s,[Set Int]) -> [String] -> Type s -> Type s
abstractImplicitType (c,is) as = mapTypeShapes g . warp i'typeRel addCons . mapTypePaths f
  where f (NamedRoot r,p) = lookup r rootInds <&> \i -> (ImplicitRoot 0,TypeIndex ct i:p)
        f (ImplicitRoot _,_) = Nothing
        f (ContextRoot _,_) = Nothing
        f x = Just x
        g _ (NativeType (NT_RigidType _)) = PolyType
        g _ x = x
        ct = ClassType (length as) is c
        rootInds = c'map (fromAList (zip as [0..]))
        addCons = lnSkel (ImplicitRoot 0,[]) (ImplicitRoot 0,[]) (Pure $ TypeCons ct)

constraintType :: Ord s => s -> Int -> Type s
constraintType t nargs = zero
                         & compose [ln' p ((poly --> poly) --> poly)
                                    . ln (p+[In,In]) (p+[In,Out]) poly
                                    . ln (p+[In,In]) (retp+[In,TypeIndex tc i]) poly
                                   | i <- [0..nargs-1]
                                   , let p = take i (repeat Out)]
                         . ln' retp (poly --> poly)
                         . ln (retp+[In]) (retp+[Out]) (Join (Compose (tc,[])))
                         & Type
  where retp = take nargs (repeat Out)
        tc = NamedType nargs t

-- | `functionFrom t` is the type (t -> *)
functionFrom :: Ord s => Int -> Type s -> Type s
functionFrom shift = mapTypePaths (Just . f)
                     >>> warp i'typeRel (ln' [] (poly --> poly))
  where f (TypeRoot,p) = (TypeRoot,In:p)
        f (ImplicitRoot r,p) = (ImplicitRoot (r+shift),p)
        f x = x

freezeType :: Ord s => Type s -> Type s
freezeType = mapTypeShapes toRigid
  where toRigid ((_,p):_) PolyType = NativeType (NT_RigidType ('*':showCs p))
        toRigid _ (SkolemType n) = NativeType (NT_RigidType ('.':show n))
        toRigid _ x = x
        showCs = intercalate "," . map showC
        showC (TypeIndex _ n) = show n
thawType :: Ord s => Type s -> Type s
thawType = mapTypeShapes fromRigid
  where fromRigid _ (NativeType (NT_RigidType s)) = case identName s of
          '*':_ -> PolyType
          '.':n -> SkolemType (read n)
          _ -> NativeType (NT_RigidType s)
        fromRigid _ x = x

builtinType :: forall s. Identifier s => Builtin -> Type s
builtinType b = (zero :: Type s) & i'typeRel %~ case b of
  B_Undefined     -> ln' [] poly
  B_Foreign _ _   -> ln' [] poly
  B_Seq           -> ln' [] (poly --> poly --> poly)
                     . ln [Out,In] [Out,Out] poly

  (B_Number _)    -> ln' [] intT
  (B_String _)    -> ln' [] stringT
  (B_Bytes _)     -> ln' [] stringT
  B_Unit          -> ln' [] unitT
  B_FileDesc _    -> ln' [] fileT

  B_Open          -> ln' [] (stringT --> (fileT --> poly) --> poly)
                     . ln [Out,In,Out] [Out,Out] poly
  B_Read          -> ln' [] (fileT --> intT --> (stringT --> poly) --> poly)
                     . ln [Out,Out,In,Out] [Out,Out,Out] poly
  B_Write         -> ln' [] (fileT --> stringT --> poly --> poly)
                     . ln [Out,Out,In] [Out,Out,Out] poly
  B_Close         -> ln' [] (fileT --> poly --> poly)
                     . ln [Out,In] [Out,Out] poly

  B_AddInt        -> ln' [] (intT --> intT --> intT)
  B_SubInt        -> ln' [] (intT --> intT --> intT)
  B_MulInt        -> ln' [] (intT --> intT --> intT)
  B_DivInt        -> ln' [] (intT --> intT --> intT)
  B_CmpInt_LT     -> ln' [] (intT --> intT --> poly --> poly --> poly)
                     . ln [Out,Out,In] [Out,Out,Out,In] poly
                     . ln [Out,Out,In] [Out,Out,Out,Out] poly
  B_CmpInt_EQ     -> ln' [] (intT --> intT --> poly --> poly --> poly)
                     . ln [Out,Out,In] [Out,Out,Out,In] poly
                     . ln [Out,Out,In] [Out,Out,Out,Out] poly

  B_AddString     -> ln' [] (stringT --> stringT --> stringT)
  B_StringLength  -> ln' [] (stringT --> intT)
  B_ShowInt       -> ln' [] (intT --> stringT)

  
  B_MkArray       -> ln' [] (intT --> arrayT)
  B_ArrayLength   -> ln' [] (arrayT --> intT)
  B_ArrayAt       -> ln' [] (arrayT --> intT --> poly)
  B_ArraySet      -> ln' [] (arrayT --> intT --> poly --> poly --> poly)
                     . ln [Out,Out,Out,In] [Out,Out,Out,Out] poly

  B_SyntaxNode    -> ln' [] (arrayT --> synT)
  B_SyntaxSym     -> ln' [] (stringT --> synT)
  B_SyntaxExpr    -> ln' [] (exprT --> synT)
  B_SyntaxInd     -> ln' [] (synT --> (arrayT --> poly) --> (exprT --> poly) --> (stringT --> poly) --> poly)
                     . ln [Out,In,Out] [Out,Out,In,Out] poly
                     . ln [Out,In,Out] [Out,Out,Out,In,Out] poly
                     . ln [Out,In,Out] [Out,Out,Out,Out] poly

  B_ExprLambda    -> ln' [] (stringT --> exprT --> exprT)
  B_ExprApply     -> ln' [] (exprT --> exprT --> exprT)
  B_ExprSym       -> ln' [] (stringT --> exprT)
  B_ExprInd       -> ln' [] (exprT --> (stringT --> exprT --> poly) --> (exprT --> exprT --> poly) --> (stringT --> poly) --> poly)
                     . ln [Out,In,Out,Out] [Out,Out,In,Out,Out] poly
                     . ln [Out,In,Out,Out] [Out,Out,Out,In,Out] poly
                     . ln [Out,In,Out,Out] [Out,Out,Out,Out] poly

  B_Relocatable _ _ _ _ -> ln' [] intT
  B_RawIndex _          -> ln' [] (intT --> intT)
  where
    nativeT = Pure . NativeType
    intT = nativeT NT_Int ; stringT = nativeT NT_String
    fileT = nativeT NT_File ; unitT = nativeT NT_Unit
    synT = nativeT NT_Syntax ; exprT = nativeT NT_Expr
    arrayT = nativeT NT_Array

newtype InstanceMap s a = InstanceMap (Map s (Map (Type s) a))
                        deriving (Eq,Ord,Generic,Semigroup,Monoid)
i'InstanceMap :: Iso (InstanceMap s a) (InstanceMap s' a') (Map s (Map (Type s) a)) (Map s' (Map (Type s') a'))
i'InstanceMap = iso InstanceMap (\(InstanceMap m) -> m)
instance Functor (InstanceMap s) where map f (InstanceMap m) = InstanceMap (map2 f m)
instance Foldable (InstanceMap s) where fold (InstanceMap m) = fold (map fold m)
instance Identifier s => Traversable (InstanceMap s) where sequence (InstanceMap m) = InstanceMap <$> traverse sequence m
instance (Identifier s,Serializable Bytes s,Serializable Bytes a) => Serializable Bytes (InstanceMap s a)
instance (Identifier s,Format Bytes s,Format Bytes a) => Format Bytes (InstanceMap s a)
instance (Identifier s,Identifier s') => HasIdents s s' (InstanceMap s a) (InstanceMap s' a) where
  ff'idents = from i'InstanceMap.i'ascList.each.
              (l'1 .+ l'2.i'ascList.each.l'1.ff'idents)

instance Identifier s => DataMap (InstanceMap s a) (s,Set Int,Type s) a where
  at (s,is,t) = from i'InstanceMap.mat s.at (mapTypeShapes f $ mapTypePaths g t)
    where f ps x | any keepPath ps = x
                 | otherwise = PolyType
          g (ImplicitRoot i,_) | i/=0 = Nothing
          g (ImplicitRoot 0,TypeIndex _ i:_) | not (i`isKeyIn`is) = Nothing
          g (TypeRoot,_:_) = Nothing
          g x = Just x
          keepPath (ImplicitRoot 0,[]) = True
          keepPath (ImplicitRoot 0,TypeIndex _ i:_) = i`isKeyIn`is
          keepPath _ = False
          
instance Identifier s => OrderedMap (InstanceMap s a) (s,Type s) a where
  ascList = from i'InstanceMap.ascList.mapping (mapping ascList)
            .iso (\l -> [((i,j),a) | (i,li) <- l, (j,a) <- li])
            (\l -> by ascList $ c'map $ fromMAList [(i,[(j,a)]) | ((i,j),a) <- l])
  nearest cmp (i,j) (InstanceMap m) = nearest cmp i m >>= \(i',m') -> nearest cmp j m' <&> \(j',a) -> ((i',j'),a)

isValidInstanceMap :: Identifier s => InstanceMap s a -> Bool
isValidInstanceMap (InstanceMap m) = Data.Map.valid m && all Data.Map.valid m
