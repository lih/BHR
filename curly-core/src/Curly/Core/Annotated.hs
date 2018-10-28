{-# LANGUAGE CPP, RecursiveDo, ViewPatterns, PatternSynonyms, ScopedTypeVariables, UndecidableInstances, ExistentialQuantification #-}
module Curly.Core.Annotated(
  module Curly.Core.Types,

  -- * Building blocks
  Symbol(..),t'Argument,t'Builtin,AnnNode(..),Strictness,ExprStrictness,noStrictness,

  -- * Annotated expressions
  Annotated(..),annType,
  AnnExpr,anonymous,

  -- * Named expressions
  NameNode,NameExpr,RawNameExpr(..),NameTail,i'NameNode,t'exprType,
  -- ** Useful pre-defined expressions
  expr_identity,expr_constant,expr_constructor,expr_destructor,

  -- * Expression post-processing
  optimize,solveConstraints
  ) where

import Definitive
import Curly.Core
import Curly.Core.Documentation
import Data.IORef
import Language.Format
import Control.DeepSeq
import Curly.Core.Types
import GHC.Conc (par)

pattern PatBuiltin :: Semantic e i (s, Free f (Symbol s')) => Type s' -> Builtin -> e
pattern PatBuiltin t b <- PatSymbol (_,Pure (Builtin t b))

pattern TypeConstraints :: Ord s => [(s, [Set Int])] -> [(s, [Set Int])] -> Type s
pattern TypeConstraints a b <- (typeConstraints -> (a,b))

-- | A symbol is either an argument referring to its nth parent, or a builtin
data Symbol s = Argument Int
              | Builtin (Type s) Builtin
              deriving Generic
instance Eq (Symbol s) where a == b = compare a b == EQ
instance Ord (Symbol s) where
  compare (Argument n) (Argument n') = compare n n'
  compare (Builtin _ b) (Builtin _ b') = compare b b'
  compare (Argument _) (Builtin _ _) = LT
  compare (Builtin _ _) (Argument _) = GT
instance (Identifier s,Identifier s') => HasIdents s s' (Symbol s) (Symbol s') where
  ff'idents k (Builtin t b) = ff'idents k t <&> \t' -> Builtin t' b
  ff'idents _ (Argument n) = pure (Argument n)
                                                       
argument :: Traversal' (Symbol s) Int
argument k (Argument n) = Argument<$>k n
argument _ x = pure x
instance Show (Symbol s) where
  show (Argument n) = "$"+show n
  show (Builtin _ b) = "#"+show b
instance Documented (Symbol s) where
  document s = Pure (show s)
instance (Serializable Bytes s,Identifier s) => Serializable Bytes (Symbol s)
instance (Format Bytes s,Identifier s) => Format Bytes (Symbol s)
instance NFData (Symbol s) where
  rnf (Argument n) = rnf n
  rnf (Builtin _ b) = rnf b

t'Argument :: Traversal' (Symbol s) Int
t'Argument k (Argument n) = Argument <$> k n
t'Argument _ x = pure x
t'Builtin :: Traversal (Type s,Builtin) (Type s',Builtin) (Symbol s) (Symbol s')
t'Builtin k (Builtin t b) = uncurry Builtin<$>k (t,b)
t'Builtin _ (Argument n) = pure (Argument n)

type ExprStrictness s = ([Strictness s],Strictness s)

data StrictnessHead s = StH_Void
                      | StH_Val Int
                      | StH_Fix s (ExprStrictness s)
                      deriving (Eq,Ord,Generic)
instance HasIdents s s' (StrictnessHead s) (StrictnessHead s') where
  ff'idents k (StH_Fix s (stss,sts)) = liftA3 (\x y z -> StH_Fix x (y,z))
                                       (k s) ((each.ff'idents) k stss)
                                       (ff'idents k sts)
  ff'idents _ StH_Void = pure StH_Void
  ff'idents _ (StH_Val n) = pure (StH_Val n)
instance Serializable Bytes s => Serializable Bytes (StrictnessHead s)
instance Format Bytes s => Format Bytes (StrictnessHead s)
instance NFData s => NFData (StrictnessHead s)

noStrictness :: Strictness s
noStrictness = HNF StH_Void []

data Strictness s = Delayed s (ExprStrictness s)
                  | HNF (StrictnessHead s) [ExprStrictness s]
                deriving (Eq,Ord,Generic)
instance Serializable Bytes s => Serializable Bytes (Strictness s)
instance Format Bytes s => Format Bytes (Strictness s)
instance NFData s => NFData (Strictness s)
instance HasIdents s s' (Strictness s) (Strictness s') where
  ff'idents k (Delayed s es) = liftA2 Delayed (k s) ((l'1.each.ff'idents .+ l'2.ff'idents) k es)
  ff'idents k (HNF h stss) = liftA2 HNF (ff'idents k h) ((each.(l'1.each.ff'idents .+ l'2.ff'idents)) k stss)
instance Identifier s => Documented (Strictness s) where
  document st = docTag' "strictness" [Pure (sh (0 :: Int) [] st)]
    where sh n env (Delayed s e) = let env' = newVar s env
                                   in paren 0 n $ format "\\%s. %s" (head env') (shS 0 env' e)
          sh _ env (HNF h []) = shH env h
          sh n env (HNF h xs) = paren 1 n $ intercalate " " (shH env h : map (shS 2 env) xs)
          shS n env ([],e) = sh n env e
          shS n env (es,e) = format "[%s] %s" (intercalate "," (map (sh n env) es)) (sh n env e)
          shH env (StH_Val n) = case drop n env of
            h:_ -> h
            _ -> show n
          shH _   StH_Void = "_"
          shH env (StH_Fix s e) = let env' = newVar s env
                                  in format "(fix %s = %s)" (head env') (shS 0 env' e)
          newVar = newVar' . identName
            where newVar' s env | elem s env = newVar' (s+"'") env
                                | otherwise = s:env
          paren n m x | (n :: Int)>=m = x
                      | otherwise = format "(%s)" x

strictnessArg :: Traversal (Int,Int) Int (Strictness s) (Strictness s)
strictnessArg k = descend 0
  where descend n (HNF (StH_Val n') ests) =
          HNF . StH_Val <$> k (n,n') <*> traverse (descendE n) ests
        descend n (HNF StH_Void ests) = HNF StH_Void <$> traverse (descendE n) ests
        descend n (HNF (StH_Fix s est) ests) = liftA2 (\e' -> HNF (StH_Fix s e'))
                                               (descendE (n+1) est)
                                               (traverse (descendE n) ests)
        descend n (Delayed s est) = Delayed s <$> descendE (n+1) est
        descendE n (sts,st) = (,) <$> traverse (descend n) sts
                                  <*> descend n st

{- | An annotated node

This type is used as a base for all fully-resolved expressions. It
helps in caching most of the intermediate steps of compiling, such as
knowing which variables are free, what size it is, or its type.
-}
data AnnNode s a = AnnNode {
  _ident,_mass :: Int, 
  _refs :: Map (Symbol s) Int,
  _type :: Type s,
  _strictness :: ExprStrictness s,
  _shape :: ExprNode s a
  }
                 deriving Generic
annShape :: Lens (ExprNode s a) (ExprNode s b) (AnnNode s a) (AnnNode s b)
annShape = lens _shape (\x y -> x { _shape = y })
annType :: Lens' (AnnNode s a) (Type s)
annType = lens _type (\x y -> x { _type = y })
instance Ord (AnnNode s a) where
  compare a b = compare (_mass a,_ident a) (_mass b,_ident b)
instance Eq (AnnNode s a) where a == b = compare a b == EQ
instance Functor (AnnNode s) where
  map = warp annShape . map
instance Foldable (AnnNode s) where
  fold = fold . _shape
instance Traversable (AnnNode s) where
  sequence = annShape sequence
instance (NFData s, NFData a) => NFData (AnnNode s a)
instance (Identifier s,Identifier s') => HasIdents s s' (AnnNode s a) (AnnNode s' a) where
  ff'idents k (AnnNode i m r t st s) = liftA4 (\t' r' st' s' -> AnnNode i m r' t' st' s')
                                       (forl ff'idents t k)
                                       (fromAList <$> forl (each.l'1.ff'idents) (ascList$^r) k)
                                       (forl ((l'1.each .+ l'2).ff'idents) st k)
                                       (forl ff'idents s k)

-- | A partially-resolved expression node
newtype NameNode s a = NameNode ((Free (AnnNode s):.:(,) s) a)
                    deriving (Functor,Foldable,Eq,Ord)
instance Traversable (NameNode s) where sequence = coerceSequence NameNode
instance (Identifier s,Identifier s') => HasIdents s s' (NameNode s a) (NameNode s' a) where
  ff'idents = from i'NameNode.i'Compose.ff'idents
-- | A NameNode's representation
i'NameNode :: Iso (NameNode s a) (NameNode t b) (Free (AnnNode s) (s,a)) (Free (AnnNode t) (t,b))
i'NameNode = i'Compose.iso NameNode (\(NameNode a) -> a)

-- | A fully-resolved, annotated expression
type AnnExpr s = Free (AnnNode s) (Symbol s)
instance Identifier s => Semantic (AnnExpr s) s (Symbol s) where
  semNode = iso go back
    where go (Pure s) = SemSymbol s
          go (Join (AnnNode { _shape = Lambda s e })) = SemAbstract s e
          go (Join (AnnNode { _shape = Apply f x })) = SemApply f x
          back (SemApply f x) = Join (AnnNode i m r t st (Apply f x))
            where (i,m,r,t,st) = applyAnns f x
          back (SemAbstract s e) = Join (AnnNode i m r t st (Lambda s e))
            where (i,m,r,t,st) = lambdaAnns s e
          back (SemSymbol s) = Pure s

type NameTail s = Free (NameNode s) (Symbol s)
-- | A partially resolved expression. Each level corresponds to a different module.
type NameExpr s = NameNode s (NameTail s)
instance forall s. Identifier s => Semantic (NameExpr s) s (s,NameTail s) where
  semNode = iso f g
    where f e = case e^..i'NameNode of
            Join (AnnNode { _shape = Apply a b }) -> SemApply (a^.i'NameNode) (b^.i'NameNode)
            Join (AnnNode { _shape = Lambda s body }) -> SemAbstract s (body^.i'NameNode)
            Pure s -> SemSymbol s
          g (SemSymbol s) = Pure s^.i'NameNode
          g (SemAbstract s e) = 
            withType (lambdaType (exprType e))
            $ abstractSyms ecs (nnAbstract s (applySyms (1,length ecs) (0,ecs) e))
            where ecs = exprClasses e
          g (SemApply a b) = 
            withType (clearContexts te)
            $ abstractSyms ecs (nnApply (applySyms (0,tot) (0,acs) a) (applySyms (0,tot) (aclen,bcs') b'))
            where ta@(TypeConstraints tacs _) = exprType a
                  tb@(TypeConstraints tbcs tbds) = exprType b
                  aclen = length tacs
                  acs = map (by l'1) tacs
                  bcs = map (by l'1) tbcs ; bds = map (by l'1) tbds
                  tb' = clearContexts tb
                  tap = applyType ta tb'
                  tcarg = selectConstraints (\n -> guard (n>=aclen) >> return (n-aclen)) Just tap
                  tcf = selectConstraints (\i -> guard (i<aclen) >> return i) (const Nothing) tap
                  (te@(TypeConstraints tecs _),b') = prepareArgument (tcf,tcarg) (stripDeducts b)
                  ecs = map (by l'1) tecs
                  bcs' = drop aclen ecs
                  tot = length ecs
          
                  prepareArgument :: (Type s,Type s) -> NameExpr s -> (Type s,NameExpr s)
                  prepareArgument (tf,t@(TypeConstraints _ td)) e
                    | empty td = (tap,e)
                    | otherwise =
                      let targ@(TypeConstraints _ tds) = freezeType t
                          im = fold
                               $ zipWith (\i (s,is) -> [((s,ind,t'),(t',mkSymbol (s,Pure (Argument i))))
                                                       | ind <- is
                                                       , let t' = mapTypePathsMonotonic (keepContext i) targ]) [0..]
                               $ reverse tds
                          keepContext i (ContextRoot j,p) | i==j = Just (ImplicitRoot 0,p)
                          keepContext _ _ = Nothing
                          solved = solveConstraints (fromAList im) (set t'exprType targ (applySyms (0,length tds) (0,[]) e))
                          tsolve = exprType solved ; csolve = map (by l'1) $ fst $ typeConstraints tsolve
                      in ( tf + thawType tsolve
                         , abstractSyms (csolve + map (by l'1) tds)
                         $ applySyms (length tds,length csolve) (0,csolve)
                         $ solved )
                  prepareArgument _ e = (tap,e)

                  stripDeducts :: NameExpr s -> NameExpr s
                  stripDeducts e
                    | nonempty bds =
                      let [sym] = map pureIdent ["a"]
                          arg = Pure . Argument
                      in set t'exprType tb'
                         $ abstractSyms (bcs+[sym]) $ nnApply (applySyms (0,1+length bcs) (1,bcs) e)
                         $ abstractSyms bds
                         $ mkSymbol (sym,arg (length bds))
                    | otherwise = e

newtype RawNameExpr s = RawNameExpr { _rawNameExpr :: NameExpr s }
instance Identifier s => Semantic (RawNameExpr s) s (s,NameTail s) where
  semNode = iso f g
    where f (RawNameExpr e) = case e^.semNode of
            SemSymbol s -> SemSymbol s
            SemApply a b -> SemApply (RawNameExpr a) (RawNameExpr b)
            SemAbstract s body -> SemAbstract s (RawNameExpr body)
          g e = RawNameExpr $ case e of
            SemSymbol s -> mkSymbol s
            SemApply (RawNameExpr a) (RawNameExpr b) -> nnApply a b
            SemAbstract s (RawNameExpr body) -> nnAbstract s body

nnApply :: Annotated (NameNode s a) s => NameNode s a -> NameNode s a -> NameNode s a
nnApply a b = Join (AnnNode i m r zero st (Apply (a^..i'NameNode) (b^..i'NameNode)))^.i'NameNode
  where (i,m,r,_,st) = applyAnns a b
nnAbstract :: Annotated (NameNode s a) s => s -> NameNode s a -> NameNode s a
nnAbstract s e = Join (AnnNode i m r zero st (Lambda s (e^..i'NameNode)))^.i'NameNode
  where (i,m,r,_,st) = lambdaAnns s e 

expr_identity :: Identifier s => RawNameExpr s
expr_identity = mkAbstract (pureIdent "#0") (mkSymbol (pureIdent "#0",Pure (Argument 0)))
expr_constant :: Identifier s => RawNameExpr s -> RawNameExpr s
expr_constant = mkAbstract (pureIdent "_")
expr_constructor :: Identifier s => Type s -> RawNameExpr s
expr_constructor t
  | empty (cs+ds) = expr_identity
  | otherwise = let cargs = map (by l'1) cs ; dargs = map (by l'1) ds
                    clen = length cargs; dlen = length dargs
                    [a,k] = map pureIdent ["a","k"]
                    arg = Pure . Argument
                in RawNameExpr $ abstractSyms (cargs+[a]+dargs+[k])
                   $ nnApply (applySyms (dlen+2,clen) (0,cargs) (mkSymbol (k,arg 0)))
                   $ applySyms (1,dlen) (0,dargs) (mkSymbol (a,arg 1))
  where TypeConstraints cs ds = t
expr_destructor :: Identifier s => Type s -> RawNameExpr s
expr_destructor t
  | empty (cs+ds) = expr_identity
  | otherwise = let args = map (by l'1) cs 
                    len = length args
                    [k,z] = map pureIdent ["k","z"]
                    arg = Pure . Argument
                in RawNameExpr $ abstractSyms (args+[k,z])
                   $ nnApply (applySyms (2,len) (0,args) (mkSymbol (z,arg 0)))
                   $ mkSymbol (k,arg 1)
  where TypeConstraints cs ds = t

withType :: Type s -> NameExpr s -> NameExpr s
withType t = warp (from i'NameNode) $ \n -> case n of
  Join n' -> Join n' { _type = t }
  Pure (s,tl) -> Pure (s,warp t'Join (withType t) tl)
abstractSyms :: Identifier s => [s] -> NameExpr s -> NameExpr s
abstractSyms l e = foldr nnAbstract e l
applySyms :: Identifier s => (Int,Int) -> (Int,[s]) -> NameExpr s -> NameExpr s
applySyms (threshold,total) (start,l) e =
  foldl' (\e' (i,s) -> nnApply e' (mkSymbol (s,Pure (Argument i))))
  (if total /= 0 && nonempty (exprRefs e)
   then mapRefs (\n n' -> if n'-n >= threshold then n'+total else n') e
   else e)
  (zip [initial-1,initial-2..] l)
  where initial = threshold+total-start

mapRefs :: Identifier s => (Int -> Int -> Int) -> NameExpr s -> NameExpr s
mapRefs sym = descend 0
  where descend n e = case e^.semNode of
          SemSymbol x -> mkSymbol $ warp (l'2.t'Pure.argument) (sym n) x
          SemAbstract s e' -> nnAbstract s (descend (n+1) e')
          SemApply f x -> nnApply (descend n f) (descend n x)

isComplexStrictness :: Eq s => Strictness s -> Bool
isComplexStrictness (HNF (StH_Val n) xs) = or [n==n' && length xs > length xs' && and (zipWith (==) xs xs')
                                            | (_,HNF (StH_Val n') xs') <- xs]
                                         || any (isComplexStrictness . snd) xs
isComplexStrictness (HNF (StH_Fix _ _) _) = True
isComplexStrictness (HNF _ xs) = any (isComplexStrictness . snd) xs
isComplexStrictness (Delayed _ e) = isComplexStrictness (snd e)

optimize :: forall s. Identifier s => (Builtin -> s) -> NameExpr s -> NameExpr s
optimize showB = \e -> if envVar "optimize" "CURLY_OPTIMIZE"=="optimize"
                       then set t'exprType (exprType e) $ _rawNameExpr $ opt ([],[]) e
                       else e
  where
    -- prettyNE e = pretty (mapParams identName (map (identName . fst) (semantic e) :: Expression s String) :: Expression String String)
    -- prettyM m = format "[%s]" (intercalate "," (map (maybe "?" prettyNE) m)) :: String
    -- prettyV v = format "[%s]" (intercalate "," (map (\(b,x) -> show b+":"+prettyNE x) v)) :: String
    opt (m,v) = sem >>> \case
      SemSymbol (s,Pure (Argument n)) ->
        let transNode d = sem >>> \case
              SemSymbol (sid,Pure (Argument n'')) | n''>=d -> mkSymbol (sid,Pure (Argument (n'+n''))) 
              SemSymbol sym -> mkSymbol sym
              SemApply f x -> mkApply (transNode d f) (transNode d x)
              SemAbstract sid e -> mkAbstract sid (transNode (d+1) e)
            n' = transTail n
        in case drop n m of
          Just x':_ -> (if nonempty v then opt ([],v) . _rawNameExpr else id) (transNode 0 x')
          _ -> foldl' mkApply (mkSymbol (s,Pure (Argument n'))) (map snd v)
      SemSymbol (s,Pure (Builtin t b)) -> optBuiltin (s,t,b) v
      SemSymbol sym@(_,Join x) -> case (leftmost x,v) of
        ((n,SemAbstract _ e'),(b,_):_) | isInline b n e' -> opt (m,v) x
        ((_,SemSymbol (_,Pure (Builtin _ _))),_) -> opt (m,v) x
        ((_,SemSymbol (_,Join _)),_) -> opt (m,v) x
        _ -> foldl' mkApply (mkSymbol sym) (map snd v)
      SemAbstract s e' -> case v of
        (b,x):v' | isInline b 0 e' -> opt (Just x:m,v') e'
                 | otherwise -> mkApply (etaReduce v' s e') x
        [] -> etaReduce [] s e'
      SemApply f x -> opt (m,(vh,x'):v) f
        where x' = opt (m,[]) x
              vh = case sem x' of SemSymbol _ -> True ; _ -> False
      where transTail n = n-length [() | Just _ <- take n m]
            isInline b n e' = b || mlookup (Argument n) (exprRefs e') <= 1
                              || (not (isComplexStrictness (snd (exprStrictness e'))))
            etaReduce v' s e = let e' = opt (Nothing:m,v') (e :: NameExpr s) in
              case sem e' of
                SemApply f (sem -> SemSymbol (_,Pure (Argument 0)))
                  | mlookup (Argument 0) (exprRefs (_rawNameExpr f)) == 0
                    -> opt ([Just undefined],[]) (_rawNameExpr f)
                _ -> mkAbstract s e'
    leftmost (sem -> y) = case y of
      SemApply f _ -> first (+1) (leftmost f)
      _ -> (0,y)
    isComm B_AddInt B_AddInt = True
    isComm B_AddString B_AddString = True
    isComm _ _ = False
    mkBuiltin t b = mkSymbol (showB b,Pure (Builtin t b))
    mkNumber t n = mkBuiltin t (B_Number n)
    optBuiltin (_,t,B_ShowInt) [(_,PatBuiltin _ (B_Number n))] = mkBuiltin t (B_String (show n))
    optBuiltin (_,_,B_AddInt) [(_,PatBuiltin t (B_Number n)),(_,PatBuiltin _ (B_Number n'))] = mkNumber t (n+n')
    optBuiltin (_,_,B_MulInt) [(_,PatBuiltin t (B_Number n)),(_,PatBuiltin _ (B_Number n'))] = mkNumber t (n*n')
    optBuiltin (_,_,B_SubInt) [(_,PatBuiltin t (B_Number n)),(_,PatBuiltin _ (B_Number n'))] = mkNumber t (n-n')
    optBuiltin (_,_,B_DivInt) [(_,PatBuiltin t (B_Number n)),(_,PatBuiltin _ (B_Number n'))] = mkNumber t (n`div`n')
    optBuiltin (s,t,B_AddInt) [(x1,PatApply2 (PatBuiltin _ B_AddInt) x (PatBuiltin ts (B_Number a)))
                              ,(x2,PatBuiltin _ (B_Number b))]
      = optBuiltin (s,t,B_AddInt) [(x1,x),(x2,mkBuiltin ts (B_Number (a+b)))]
    
    optBuiltin (_,_,B_AddString) [(_,PatBuiltin t (B_String x)),(_,PatBuiltin _ (B_String y))] = mkBuiltin t (B_String (x+y))
    optBuiltin (s,t,B_AddString) [(x1,PatApply2 (PatBuiltin _ B_AddString) x (PatBuiltin ts (B_String a)))
                                 ,(x2,PatBuiltin _ (B_String b))]
      = optBuiltin (s,t,B_AddString) [(x1,x),(x2,mkBuiltin ts (B_String (a+b)))]

    optBuiltin (s,t,b) [(x1,x),(x2,PatApply2 (PatBuiltin _ b') y z)]
      | isComm b b' = optBuiltin (s,t,b) [(x1,opt ([],[]) $ _rawNameExpr (mkBuiltin t b'`mkApply`x`mkApply`y))
                                         ,(x2,z)]
    optBuiltin (s,t,b) v = foldl' mkApply (mkSymbol (s,Pure (Builtin t b))) (map snd v)

solveConstraints :: Identifier s => InstanceMap s (Type s,NameExpr s) -> NameExpr s -> NameExpr s
solveConstraints im = solve
  where solve e = 
          let ti = exprType e
              insts t =
                let TypeConstraints cs _ = t
                in zipWith (\j (i,x) -> (i-j,x)) [0..]
                   [(i,e') | (i,(c,is)) <- zip [0..] cs
                           , let t' = mapTypePathsMonotonic (keepRoot i (const True)) t
                           , e' <- take 1 [
                              e'
                              | ind <- is
                              , Just e' <- [lookup (c,ind,ti) im]
                              , t'`isSubtypeOf`mapTypePathsMonotonic (keepRoot 0 (`isKeyIn`ind)) (fst e')]]
              keepRoot i keep (ImplicitRoot j,p) | i==j = case p of
                [] -> Just (ImplicitRoot 0,p)
                TypeIndex _ n:_ | keep n -> Just (ImplicitRoot 0,p)
                _ -> Nothing
              keepRoot _ _ _ = Nothing
              solve1 (ta,ea) (i,(tb,eb)) =
                (tret,) $ abstractSyms csret
                $ applySyms (retl,0) (i,act)
                $ (applySyms (0,retl) (0,ach) ea 
                   `nnApply`
                   applySyms (0,retl) (length acs-1,bcs) eb)
                where TypeConstraints tbcs _ = tb
                      acs = let TypeConstraints cs _ = ta in map (by l'1) cs
                      _:bcs = map (by l'1) tbcs
                      (ach,_:act) = splitAt i acs 
                      csret = ach+act+bcs
                      retl = length csret
                      dropRoot (TypeRoot,_) = Nothing
                      dropRoot x = Just x
                      dropIthClass (ImplicitRoot j,p) = case compare i j of
                        EQ -> Nothing
                        LT -> Just (ImplicitRoot (j-1),p)
                        GT -> Just (ImplicitRoot j,p)
                      dropIthClass x = Just x
                      adjustClass (ImplicitRoot 0,p) = Just (ImplicitRoot i,p)
                      adjustClass (ImplicitRoot j,p) = Just (ImplicitRoot (length acs+j-1),p)
                      adjustClass x = Just x
                      tret = let tb' = mapTypePathsMonotonic (dropRoot >=> adjustClass) tb
                             in mapTypePathsMonotonic dropIthClass (tb' + ta)
          in case insts ti of
            [] -> e
            l -> solve (foldl' solve1 (ti,e) l & \(ti',e') -> set t'exprType ti' e')

exprClasses :: Identifier s => NameExpr s -> [s]
exprClasses e = exprType e & \ ~(TypeConstraints cs _) -> map (by l'1) cs

-- | Remove all naming information from an expression.
anonymous :: NameExpr s -> AnnExpr s
anonymous e = e^..i'NameNode >>= \(_,t) -> case t of Pure s -> pure s; Join e' -> anonymous e'

shape :: Iso' Int (Symbol s:+:ExprNode () Int)
shape = by thunk $ do
  v <- newIORef (zero :: (Int,Bimap Int (Symbol s:+:ExprNode () Int)))
  let getS n = by thunk $ do
        (_,m) <- readIORef v
        return (fromMaybe (Left (Builtin undefined B_Undefined)) (m^.at n))
      getI s = deepseq s $ by thunk $ runAtomic v $ get >>= \(e,m) -> case m^.commuted.at s of
        Just n -> return n
        _ -> e <$ put (e+1,insert e s m)
  return $ iso getS getI

-- | The class of annotated expressions, from which some information may be retrieved.
class Identifier s => Annotated e s | e -> s where
  exprIdent :: e -> Int
  exprMass :: e -> Int
  exprRefs :: e -> Map (Symbol s) Int
  exprType :: e -> Type s
  exprStrictness :: e -> ExprStrictness s
instance Identifier s => Annotated (AnnExpr s) s where
  exprIdent (Join ann) = _ident ann
  exprIdent (Pure s) = Left s^..shape
  exprMass (Join ann) = _mass ann
  exprMass (Pure _) = 1
  exprRefs (Join ann) = _refs ann
  exprRefs (Pure s@(Argument _)) = singleton s 1
  exprRefs (Pure _) = zero
  exprType (Join ann) = _type ann
  exprType (Pure (Argument n)) = argumentType n
  exprType (Pure (Builtin t _)) = t
  exprStrictness (Join ann) = _strictness ann
  exprStrictness (Pure (Argument n)) = pure (HNF (StH_Val n) [])
  exprStrictness (Pure (Builtin _ b)) = pure $ case b of
    B_AddInt -> binOp 
    B_MulInt -> binOp 
    B_DivInt -> binOp 
    B_SubInt -> binOp 
    B_AddString -> binOp
    B_StringLength -> monOp
    B_ShowInt -> monOp
    B_CmpInt_LT -> binOp
    B_CmpInt_EQ -> binOp
    _ -> HNF StH_Void []
    where binOp = Delayed (pureIdent "x") $ pure $ Delayed (pureIdent "y") $ do
            tell [arg 0, arg 1]
            pure (HNF StH_Void [])
          monOp = Delayed (pureIdent "x") $ do 
            tell [arg 0]
            pure (HNF StH_Void [])
          arg n = HNF (StH_Val n) []

nameProp :: (forall b. AnnNode s b -> a) -> (AnnExpr s -> a) -> NameExpr s -> a
nameProp np anp = fix $ \nnp a -> case a^..i'NameNode of
  Join ann -> np ann
  Pure (_,Join t) -> nnp t
  Pure (_,Pure x) -> anp (Pure x)

instance Identifier s => Annotated (NameExpr s) s where
  exprIdent = nameProp _ident exprIdent
  exprMass = nameProp _mass exprMass
  exprRefs = nameProp _refs exprRefs
  exprType = nameProp _type exprType
  exprStrictness = nameProp _strictness exprStrictness

t'exprType :: Fold' (NameExpr s) (Type s)
t'exprType = fix $ \node -> from i'NameNode.(t'Join.annType
                                             .+ t'Pure.l'2.(t'Join.node .+ t'Pure.argType))
  where argType k (Builtin t b) = k t <&> \t' -> Builtin t' b
        argType _ (Argument n) = pure (Argument n)

lambdaAnns :: forall e s. Annotated e s => s -> e -> (Int,Int,Map (Symbol s) Int,Type s,ExprStrictness s)
lambdaAnns s e = (i,m,r,lambdaType (exprType e),pure (Delayed s (exprStrictness e)))
  where i = Right (Lambda () (exprIdent e))^..shape
        m = exprMass e + 1
        r = delete (Argument 0) (exprRefs e) & ascList.each.l'1.argument %~ subtract 1
        
lambdaType :: forall s. Identifier s => Type s -> Type s
lambdaType te = extractFirstArgument te

applyAnns :: forall e s. Annotated e s => e -> e -> (Int,Int,Map (Symbol s) Int,Type s,ExprStrictness s)
applyAnns a b = (i,m,r,applyType (exprType a) (exprType b),st)
  where i = Right (Apply (exprIdent a) (exprIdent b))^..shape
        m = exprMass a + exprMass b + 1
        r = exprRefs a *+ exprRefs b
        st = applyStrictness (exprStrictness b) =<< exprStrictness a
applyType :: forall s. Identifier s => Type s -> Type s -> Type s
applyType ta tb = tret
  where tret = force ta`par`force tb`par`mapTypePathsMonotonic dropTop tsum
        ~(hasErr,tsum) = traverseTypeShapes go (functionFrom (length (fst (typeConstraints ta))) tb + ta)
          where go ps x@(TypeMismatch _ _) = tell (any isDeleted ps) >> return x
                go ps x = pure (if hasErr && (TypeRoot,[Out])`elem`ps then HiddenTypeError else x)
                isDeleted (TypeRoot,[]) = True
                isDeleted (TypeRoot,In:_) = True
                isDeleted _ = False
        dropTop (TypeRoot,[]) = Nothing
        dropTop (TypeRoot,(In:_)) = Nothing
        dropTop (TypeRoot,Out:p) = Just (TypeRoot,p)
        dropTop x = Just x
        
applyStrictness :: forall s. Eq s => ExprStrictness s -> Strictness s -> ExprStrictness s
applyStrictness esb sa = case sa of
  Delayed _ est -> traverseSt (substD Nothing 0) est
  HNF x l -> pure (HNF x (l+[esb]))
  where traverseSt :: (Strictness s -> ExprStrictness s) -> ExprStrictness s -> ExprStrictness s
        traverseSt k (es,e) = (tell =<< traverse k es) >> k e

        substD_abs isFix n = substD (map (+1) isFix) (n+1)
        
        substD isFix n (Delayed s est) = pure (Delayed s (traverseSt (substD_abs isFix n) est))
        substD isFix n (HNF h ests) = case h of
          StH_Val arg ->
            case (or [arg==arg''
                      && length ests > length ests''
                      && all (uncurry (==)) (zip ests ests'')
                     | (_,HNF (StH_Val arg'') ests'') <- ests],
                  compare arg n,
                  snd esb) of
              (False,EQ,_) ->
                foldl' (\esf esx -> do
                           sf <- esf
                           applyStrictness (traverseSt (substD isFix n) esx) sf)
                (esb & (l'1.each .+ l'2).strictnessArg %~ \(d,v) -> if v >= d then v+n else v)
                ests
              (True,EQ,Delayed s e) -> pure $ case isFix of
                Just m -> HNF (StH_Val m) []
                Nothing -> HNF (StH_Fix s (traverseSt (substD (Just 0) n) e)) (drop 1 ests')
              (_,cmpn,_) -> pure $ HNF (StH_Val (maybe 0 (\m -> if arg>=m then 1 else 0) isFix
                                                 + if cmpn==LT then arg else arg-1)) ests'
          StH_Void -> pure $ HNF StH_Void []
          StH_Fix s e -> pure $ HNF (StH_Fix s (traverseSt (substD_abs isFix n) e)) ests'
          where ests' = map (traverseSt (substD isFix n)) ests

        
