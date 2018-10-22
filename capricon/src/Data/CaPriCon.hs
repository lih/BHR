{-# LANGUAGE UndecidableInstances, OverloadedStrings, NoMonomorphismRestriction, DeriveGeneric, ConstraintKinds #-}
module Data.CaPriCon(
  -- * Expression nodes
  IsCapriconString(..),BindType(..),Node(..),ApHead(..),Application(..),COCExpression(..),
  -- ** Managing De Bruijin indices
  adjust_depth,adjust_telescope_depth,inc_depth,free_vars,is_free_in,
  -- ** General term substitution, type inference and convertibility
  subst,substn,type_of,mu_type,convertible,
  -- ** Expression directories
  StringPattern,NodeDir(..),AHDir(..),ApDir,
  findPattern,freshContext,
  -- * Showing nodes
  NodeDoc(..),doc2raw,doc2latex,showNode,showNode'
  ) where

import Definitive
import Language.Format
import GHC.Generics (Generic)

sequence2 :: (Applicative f,Traversable t,Traversable u) => t (u (f a)) -> f (t (u a))
sequence2 = sequence.map sequence
sequence3 :: (Applicative f,Traversable t,Traversable u,Traversable v) => t (u (v (f a))) -> f (t (u (v a)))
sequence3 = sequence.map sequence2

type FreeMap f a = Cofree f (Maybe a)
instance (Semigroup a, Semigroup (Coforest f a)) => Semigroup (Cofree f a) where
  Step a b + Step a' b' = Step (a+a') (b+b')
instance (Monoid a, Monoid (Coforest f a)) => Monoid (Cofree f a) where
  zero = Step zero zero
instance (DataMap (f (FreeMap f a)) k (FreeMap f a)) => DataMap (FreeMap f a) [k] a where
  at [] = lens (\(Step a _) -> a) (\(Step _ m) a -> Step a m)
  at (h:t) = coforest.at h.l'Just zero.at t
coforest :: Lens' (Cofree f a) (Coforest f a)
coforest = lens (\(Step _ m) -> m) (\(Step a _) m -> Step a m)

class (Ord str,Show str,Monoid str,Sequence str,IsString str) => IsCapriconString str where
  toString :: str -> String
instance IsCapriconString String where
  toString = id

type ListStream = [Word8]
type ListBuilder = ListStream -> ListStream
instance SerialStream Word8 ListBuilder ListStream where
  encodeByte _ b = (b:)
  toSerialStream k = k []

-- | Inductive types
data BindType = Lambda | Prod
              deriving (Show,Eq,Ord,Generic)
data Node str = Bind BindType str (Node str) (Node str)
              | Cons (Application str)
              | Universe Int
          deriving (Show,Generic)
data ApHead str = Sym Int | Mu [(str,Node str,Node str)] [Node str] (Application str)
            deriving (Show,Generic)
data Application str = Ap (ApHead str) [Node str]
                 deriving (Show,Generic) 
type Env str = [Node str]

type ListSerializable a = (Serializable Word8 ListBuilder ListStream a)
type ListFormat a = (Format Word8 ListBuilder ListStream a)
instance ListSerializable BindType
instance ListFormat BindType
instance ListSerializable str => ListSerializable (Node str)
instance ListFormat str => ListFormat (Node str)
instance ListSerializable str => ListSerializable (ApHead str)
instance ListFormat str => ListFormat (ApHead str)
instance ListSerializable str => ListSerializable (Application str)
instance ListFormat str => ListFormat (Application str)

class Monad m => COCExpression str m e | e -> str where
  mkUniverse :: Int -> m e
  mkVariable :: Int -> m e
  mkBind :: BindType -> str -> e -> e -> m e
  mkApply :: e -> e -> m e
instance (IsCapriconString str,Monad m) => COCExpression str m (Node str) where
  mkUniverse = pure . Universe
  mkVariable = pure . Cons . \i -> Ap (Sym i) []
  mkBind b x tx e = pure $ Bind b x tx e
  mkApply f x = return (subst f (Cons (Ap (Sym 0) [inc_depth 1 x])))

data ContextNode str = ContextNode Int (Node str)
rawNode (ContextNode _ x) = x
inContext :: MonadReader [(str,Node str)] m => ContextNode str -> m (ContextNode str)
inContext (ContextNode d e) = ask <&> \(length -> nctx) -> ContextNode nctx (inc_depth (nctx-d) e)

instance (IsCapriconString str,MonadReader [(str,Node str)] m,Monad m) => COCExpression str m (ContextNode str) where
  mkUniverse u = ask >>= \ctx -> ContextNode (length ctx)<$>mkUniverse u
  mkVariable i = ask >>= \ctx -> ContextNode (length ctx)<$>mkVariable i
  mkBind t x tx e = do
    ContextNode dr tx' <- inContext tx
    e' <- rawNode <$> local ((x,tx'):) (inContext e)
    ContextNode dr <$> mkBind t x tx' e'
  mkApply cf cx = do
    ContextNode dr f <- inContext cf
    x <- rawNode <$> inContext cx
    ContextNode dr <$> mkApply f x

data NodeDir str a = NodeDir
  (Map BindType (NodeDir str (NodeDir str a)))
  (ApDir str a)
  (Map Int a)
  deriving (Eq,Ord,Show,Generic)
instance Functor (NodeDir str) where
  map f (NodeDir a b c) = NodeDir (map3 f a) (map3 f b) (map f c)
instance Foldable (NodeDir str) where
  fold (NodeDir a b c) = (fold.map fold.map2 fold) a + (fold.map fold.map2 fold) b + fold c
instance Traversable (NodeDir str) where
  sequence (NodeDir a b c) = NodeDir<$>sequence3 a<*>sequence3 b<*>sequence c
instance (ListSerializable str, ListSerializable a) => ListSerializable (NodeDir str a)
instance (ListFormat str, ListFormat a) => ListFormat (NodeDir str a)

i'NodeDir :: Iso (NodeDir str a) (NodeDir str' a')
             ((,,) (Map BindType (NodeDir str (NodeDir str a)))
               (ApDir str a)
               (Map Int a))
             ((,,) (Map BindType (NodeDir str' (NodeDir str' a')))
               (ApDir str' a')
               (Map Int a'))
i'NodeDir = iso (\(x,y,z) -> NodeDir x y z) (\(NodeDir x y z) -> (x,y,z))

type ApDir str a = AHDir str (FreeMap (NodeDir str) a)
data AHDir str a = AHDir
  (Map Int a)
  (Map Int (ApDir str a))
  deriving (Eq,Ord,Show,Generic)
instance Functor (AHDir str) where
  map f (AHDir a b) = AHDir (map f a) ((map2.map2) f b)
instance Foldable (AHDir str) where
  fold (AHDir a b) = fold a + (fold.map fold.map2 fold.map3 fold) b
instance Traversable (AHDir str) where
  sequence (AHDir a b) = AHDir<$>sequence a<*>(sequence3.map3 sequence) b
instance (ListSerializable str, ListSerializable a) => ListSerializable (AHDir str a)
instance (ListFormat str, ListFormat a) => ListFormat (AHDir str a)
i'AHDir :: Iso (AHDir str a) (AHDir str' a')
           ((,) (Map Int a) (Map Int (ApDir str a)))
           ((,) (Map Int a') (Map Int (ApDir str' a')))
i'AHDir = iso (uncurry AHDir) (\(AHDir x y) -> (x,y))

i'Cofree :: Iso (Cofree f a) (Cofree f' a') (a,Coforest f a) (a',Coforest f' a')
i'Cofree = iso (uncurry Step) (\(Step x y) -> (x,y))

instance Semigroup (NodeDir str a) where NodeDir a b c + NodeDir a' b' c' = NodeDir (a+a') (b+b') (c+c')
instance Monoid (NodeDir str a) where zero = NodeDir zero zero zero
instance DataMap (NodeDir str a) (Node str) a where
  at (Bind t _ tx e) = from i'NodeDir.l'1.at t.l'Just zero.at tx.l'Just zero.at e
  at (Cons a) = from i'NodeDir.l'2.atAp a
  at (Universe u) = from i'NodeDir.l'3.at u

instance Semigroup (AHDir str a) where AHDir a b + AHDir a' b' = AHDir (a+a') (b+b')
instance Monoid (AHDir str a) where zero = AHDir zero zero
instance DataMap (AHDir str a) (ApHead str) a where
  at (Sym i) = from i'AHDir.l'1.at i
  at (Mu xs _ a) = from i'AHDir.l'2.at (length xs).l'Just zero.atAp a

type StringPattern str = [str :+: Int]

atAp :: Application str -> Lens' (ApDir str a) (Maybe a)
atAp (Ap h xs) = at h.l'Just zero.at xs

mayChoose (Just x) = return x
mayChoose Nothing = zero

(<++>) :: WriterT w [] a -> WriterT w [] a -> WriterT w [] a
a <++> b = a & from writerT %~ (+ b^..writerT)

findPattern :: NodeDir str a -> Node str -> [([([(str,Node str)],Int,Node str)],a)]
findPattern = \x y -> go [] x y^..writerT
  where go :: [(str,Node str)] -> NodeDir str a -> Node str -> WriterT [([(str,Node str)],Int,Node str)] [] a
        go_a :: [(str,Node str)] -> ApDir str a -> Application str -> WriterT [([(str,Node str)],Int,Node str)] [] a
        go_ah :: [(str,Node str)] -> AHDir str a -> ApHead str -> WriterT [([(str,Node str)],Int,Node str)] [] a
        withEnv env d x m = foldr (\(i,as) ma -> ma <++> (foldl'.foldl') (\l a -> (tell [(env,i-length env,x)] >> return a) <++> l) zero as)
                            m (d^??from i'NodeDir.l'2.from i'AHDir.l'1.ascList.each.sat ((>=length env) . fst))
        go env d wh@(Bind t x tx e) = withEnv env d wh $ do
          d' <- mayChoose (d^.from i'NodeDir.l'1.at t)
          d'' <- go env d' tx
          go ((x,tx):env) d'' e
        go env d wh@(Cons a) = withEnv env d wh $ go_a env (d^.from i'NodeDir.l'2) a
        go env d wh@(Universe u) = withEnv env d wh $ mayChoose (d^.from i'NodeDir.l'3.at u)

        go_a env d (Ap ah xs) = do
          d' <- go_ah env d ah
          foldr
            (\x ma d'' -> ma =<< go env (d''^.from i'Cofree.l'2) x)
            (\d''' -> mayChoose (d'''^.from i'Cofree.l'1))
            xs d' 

        go_ah env d (Sym i) | i<length env = mayChoose (d^.from i'AHDir.l'1.at i)
                            | otherwise = []^.writerT
        go_ah env d (Mu tenv _ a) = do
          d' <- mayChoose (d^.from i'AHDir.l'2.at (length tenv))
          go_a env d' a

-- `adjust_depth f e` produces an expression `e'` whose variables (de
-- Bruijin indices) are adjusted from `e` by the function `f`.
--
-- `f` takes two arguments `i` and `d`, where `i` is the previous
-- variable depth, and `d` is the current depth of the considered node
-- (the number of binders between the top-level and the node in
-- question).
--
-- For example, `adjust_depth (\i d -> i-d+1) (Bind Lambda "x" (Universe 0) (Cons (Ap (Sym 1) [])))
--               == Bind Lambda "x" (Universe 0) (Cons (Ap (Sym 2) []))`

adjust_depth f = go 0
  where go d (Bind t x tx e) = Bind t x (go d tx) (go (d+1) e)
        go _ (Universe u) = Universe u
        go d (Cons a) = Cons (go_a d a)
        go_a d (Ap (Sym i) subs) | i<d = Ap (Sym i) (map (go d) subs)
                                 | otherwise = Ap (Sym (d+f (i-d))) (map (go d) subs)
        go_a d (Ap (Mu env t a') subs) = Ap (Mu
                                            (reverse $ zipWith (\i (x,tx,tx') -> (x,go (d+i) tx,go (d+i) tx')) [0..] (reverse env))
                                            (zipWith (\i -> go (d+length env+i)) [0..] t)
                                            (go_a (d+length env) a')) (map (go d) subs)
inc_depth 0 = \x -> x
inc_depth dx = adjust_depth (+dx)
adjust_telescope_depth field f = zipWith (field . adjust_depth . \i j -> if j<i then j else i+f (j-i)) [0..]
free_vars :: Node str -> Set Int
free_vars (Bind _ _ tx e) = free_vars tx + delete (-1) (map (subtract 1) (free_vars e))
free_vars (Cons a) = freeA a
  where freeA (Ap (Sym i) xs) = singleton' i + foldMap free_vars xs
        freeA (Ap (Mu env _ a') xs) = foldMap free_vars xs +
          map (subtract envS) (freeA a' - fromKList [0..envS-1])
          where envS = length env
free_vars _ = zero

is_free_in :: Int -> Node str -> Bool
is_free_in = map2 not go
  where go v (Bind _ _ t e) = go v t && go (v+1) e
        go v (Cons a) = go_a v a
        go _ (Universe _) = True
        go_a v (Ap (Sym v') subs) = v/=v' && all (go v) subs
        go_a v (Ap (Mu env _ a) subs) = go_a (v+length env) a && all (go v) subs

subst :: Show str => Node str -> Node str -> Node str
subst = flip substn 0
substn :: Show str => Node str -> Int -> Node str -> Node str
substn val n | n>=0 = getId . go n
             | otherwise = error "'subst' should not be called with a negative index"
  where go d (Bind t x tx e) = do
          tx' <- go d tx
          Bind t x tx' <$> go (d+1) e
        go _ (Universe u) = pure (Universe u)
        go d (Cons a) = go_a d a

        go_a d (Ap (Sym i) xs) = traverse (go d) xs >>= \xs' ->
          case compare i d of
            EQ -> rec_subst xs' (inc_depth d val)
            LT -> return $ Cons $ Ap (Sym i) xs'
            GT -> return $ Cons $ Ap (Sym (i-1)) xs'
        go_a d (Ap (Mu e t a) xs) = do
          x <- go_a (d+length e) a
          xs' <- traverse (go d) xs
          e' <- sequence $ reverse $ zipWith (\d' (y,ty,ty') -> liftA2 (y,,) (go (d+d') ty) (go (d+d') ty'))
                [0..] (reverse e)
          t' <- sequence $ zipWith (\d' -> go (d+d')) [length e..] t
          case x of
            Cons a' -> return (Cons (Ap (Mu e' t' a') xs'))
            _ -> rec_subst xs' =<< go_mu d e' t' x
  
        go_mu d env (tx':t) (Bind Lambda x tx e) = go_mu d ((x,tx,tx'):env) t e
        go_mu _ env _ (Cons (Ap (Sym i) xs))
          | i < length env = do
              let envS = length env
                  muEnv = reverse $ map (by l'3) env
              a' <- Cons . Ap (Sym i) <$>
                sequence (fold [if nonempty (free_vars x - fromKList [0..envS-1])
                                then [ return $ inc_depth envS $ foldl' (\e (x',tx,_) -> Bind Lambda x' tx e) x env
                                     , return $ subst x (Cons (Ap (Mu [] muEnv (Ap (Sym 0) [])) [Cons (Ap (Sym j) []) | j <- reverse [1..envS]]))]
                                else [return x]
                               | x <- xs])
              return $ foldl' (\e (x,_,tx) -> Bind Lambda x tx e) a' env
        go_mu _ e t (Cons a) = return $ Cons (Ap (Mu e t a) [])
        go_mu _ _ _ x' = error $ "Cannot produce an induction principle for a term : "+show x'

        rec_subst (y:t) (Bind Lambda _ _ e) = rec_subst t (subst y e)
        rec_subst xs (Cons (Ap h hxs)) = return (Cons (Ap h (hxs+xs)))
        rec_subst [] x = return x
        rec_subst _ x = error $ "Invalid substitution of non-lambda expression : "+show x

fresh env v = head $ select (not . (`elem` env)) (v:[v+fromString (show i) | i <- [0..]])
freshContext = go []
  where go env ((n,v):t) = let n' = fresh env n in (n',(n,v)):go (n':env) t
        go _ [] = []

data NodeDoc str = DocSeq [NodeDoc str]
                 | DocParen (NodeDoc str)
                 | DocMu (NodeDoc str)
                 | DocSubscript (NodeDoc str) (NodeDoc str)
                 | DocAssoc str (NodeDoc str)
                 | DocText str
                 | DocArrow
                 | DocSpace
                 deriving Show
par lvl d msg | d>lvl = DocParen msg
              | otherwise = msg

instance IsString str => IsString (NodeDoc str) where fromString = DocText . fromString

doc2raw :: IsCapriconString str => NodeDoc str -> str
doc2raw (DocSeq l) = fold (map doc2raw l)
doc2raw (DocParen p) = "("+doc2raw p+")"
doc2raw (DocMu m) = "μ("+doc2raw m+")"
doc2raw (DocSubscript v x) = doc2raw v+doc2raw x
doc2raw (DocAssoc x v) = "("+x+" : "+doc2raw v+")"
doc2raw DocArrow = " -> "
doc2raw (DocText x) = x
doc2raw DocSpace = " "

doc2latex :: IsCapriconString str => NodeDoc str -> str
doc2latex (DocSeq l) = fold (map doc2latex l)
doc2latex (DocParen p) = "("+doc2latex p+")"
doc2latex (DocMu m) = "\\mu("+doc2latex m+")"
doc2latex (DocSubscript v x) = doc2latex v+"_{"+doc2latex x+"}"
doc2latex (DocAssoc x v) = "("+x+":"+doc2latex v+")"
doc2latex DocArrow = " \\rightarrow "
doc2latex (DocText x) = x
doc2latex DocSpace = "\\,"

showNode = showNode' zero
showNode' :: IsCapriconString str => NodeDir str ([str],StringPattern str) -> [(str,Node str)] -> Node str -> NodeDoc str
showNode' dir = go 0
  where go d env x | Just ret <- toPat d env x = ret
        go _ _ (Universe u) = DocSubscript "Set" (fromString (show u))
        go d env whole@(Bind t aname atype body) | t == Lambda || 0`is_free_in`body = par 0 d $ DocSeq (DocText (bind_head t):drop 1 (bind_tail env whole))
                                                 | otherwise = par 0 d $ DocSeq [go 1 env atype,DocArrow,go 0 ((aname,atype):env) body]
          where bind_head Lambda = "λ"
                bind_head Prod = "∀"
                bind_tail env' x | Just ret <- toPat 0 (env'+env) x = [",",DocSpace,ret]
                bind_tail env' (Bind t' x tx e) | t==t' && (t==Lambda || 0`is_free_in`e) =
                                                  [DocSpace,DocAssoc x' (go 0 env' tx)] + bind_tail ((x',tx):env') e
                  where x' = fresh (map fst env') x
                bind_tail env' x = [",",DocSpace,go 0 env' x]
        go d env (Cons a) = showA d a
          where showA _ (Ap h xs) =
                  let ni = case h of
                             Sym i -> DocText $ case drop i env of
                               (h',_):_ -> h'
                               _ -> "#"+fromString (show i)
                             Mu _ _ a' -> DocMu (showA 0 a')
                      lvl = if empty xs then 1000 else 1
                  in par lvl d $ DocSeq $ intercalate [DocSpace] $ map pure (ni:map (go 2 env) xs)

        toPat d env x
          | (pats,(_,k)):_ <- findPattern dir x =
              let holes = c'map $ fromAList [(i,(env',hole)) | (env',i,hole) <- pats] in
                Just $ par (if all (has t'1) k then 1000 else 1 :: Int) d $ DocSeq $ intercalate [DocSpace] $ map pure $
                [case word of
                   Left w -> DocText w
                   Right i | Just (env',hole) <- lookup i holes ->
                               go 2 env $
                               let (hole',env'') =
                                     fix (\kj -> \case
                                             (Cons (Ap h t@(_:_)),_:env0)
                                               | Cons (Ap (Sym 0) []) <- last t
                                               , not (is_free_in 0 (Cons (Ap h (init t))))
                                                 -> kj (inc_depth (-1) (Cons (Ap h (init t))),env0)
                                             (Cons (Ap (Sym j') []),_:env0) | j'>0 -> kj (Cons (Ap (Sym (j'-1)) []),env0)
                                             e -> e) (hole,env')
                               in foldl' (\e (n,t) -> Bind Lambda n t e) hole' env''
                           | otherwise -> DocText "?"
                | word <- k]
          | otherwise = Nothing

type_of :: (IsCapriconString str,MonadReader (Env str) m) => Node str -> m (Maybe (Node str))
type_of = yb maybeT . go
  where go (Bind Lambda x tx e) = Bind Prod x tx <$> local (tx:) (go e)
        go (Bind Prod _ tx e) = do
          a <- go tx
          b <- local (tx:) $ go e
          case (a,b) of
            (Universe ua,Universe ub) -> return (Universe (max ua ub))
            _ -> zero
        go (Universe u) = return (Universe (u+1))
        go (Cons a) = go' a
          where go' (Ap (Sym i) subs) = do
                  e <- ask
                  case drop i e of
                    ti:_ -> rec_subst subs (inc_depth (i+1) ti)
                    _ -> zero
                go' (Ap (Mu env _ a') subs) = do
                  ta <- local (map (by l'2) env +) (go' a')
                  preret <- maybeT $^ mu_type $ foldl' (\e (x,tx,_) -> Bind Prod x tx e) ta env
                  rec_subst subs (subst (Cons a') preret)
                      
                rec_subst (y:t) (Bind Prod _ _ e) = rec_subst t (subst y e)
                rec_subst [] x = return x
                rec_subst _ _ = zero

mu_type :: MonadReader (Env str) m => Node str -> m (Maybe (Node str))
mu_type (inc_depth 1 -> root_type) = yb maybeT $ go 0 root_type
  where
    root_args = go' root_type
      where go' (Bind Prod x tx e) = (x,tx):go' e
            go' _ = []
    nargs = length root_args
    bind t = flip $ foldr (\(x,tx) e -> Bind t x tx e) 
    constr_ind d d' i = d' <= i && i < d+d'

    go d (Bind Prod x tx e) = do
      tx' <- go_col d x tx
      e' <- local (tx:) (go (d+1) e)
      return (Bind Prod x tx' e')
    go _ (Cons (Ap (Sym i) args)) = return $ Cons (Ap (Sym i) $ args + [Cons (Ap (Sym nargs) [])])
    go _ _ = zero

    go_col d xn = go_col' 0 (c'set zero)
      where go_col' d' recs (Bind Prod x tx@(Cons (Ap (Sym i) subs)) e)
              | constr_ind d d' i = do
                  let tx' = bind Prod (adjust_telescope_depth second (+(d+d')) root_args)
                            (adjust_depth (\i' -> if constr_ind d d' i' then (i'-d')+(nargs-d) else i'+nargs) tx)
                      tIx = Cons $ Ap (Sym (i+1)) $ map (inc_depth 1) subs + [Cons (Ap (Sym 0) [])]
                  e' <- local ((tx:) . (undefined:)) (go_col' (d'+2) (touch (1 :: Int) (map (+2) recs))
                                                      (adjust_depth (\j -> if j==0 then j else j+1) e))
                  return $ Bind Prod x tx' (Bind Prod x tIx e')
            go_col' d' recs (Bind Prod x tx e) = Bind Prod x tx <$> local (tx:) (go_col' (d'+1) (map (+1) recs) e)
            go_col' d' recs (Cons (Ap (Sym i) xs))
              | constr_ind d d' i = do
                  let args = reverse $ select (not . (`isKeyIn`recs)) [0..d'-1]
                      lastE = bind Lambda (adjust_telescope_depth second (+(d+d')) root_args)
                              (Cons (Ap (Sym (nargs-d-1))
                                     [Cons (Ap (Sym (j'+nargs)) args')
                                     | j <- args
                                     , let (j',args') | (j+1)`isKeyIn`recs = (j+1,[Cons (Ap (Sym k) []) | k <- reverse [0..nargs-1]])
                                                      | otherwise = (j,[])
                                     ]))
                  return $ Cons (Ap (Sym i) $ xs+[lastE])
                  
            go_col' d' _ (Universe u) = do
              let tIH = bind Prod (adjust_telescope_depth second (+(d+d')) root_args) ihRoot
                  ihRoot = Cons (Ap (Sym (nargs-d-1)) [Cons (Ap (Sym (j+nargs)) []) | j <- reverse [0..d'-1]])
              return $ Bind Prod xn tIH (Universe (u+1))
            go_col' _ _ _ = zero

convertible :: Node str -> Node str -> Maybe (Int,Int)
convertible = \x y -> map ((getMax<#>getMax) . fst) ((tell (Max 0,Max 0) >> go False x y)^..writerT)
  where go inv (Bind b _ tx e) (Bind b' _ tx' e') = guard (b==b') >> go (not inv) tx tx' >> go inv e e'
        go inv (Cons ax) (Cons ay) = go_a inv ax ay
        go inv (Universe u) (Universe v) | u>v = tellInv inv (Max (u-v),zero)
                                         | otherwise = return ()
        go _ _ _ = lift Nothing
        
        go_a inv (Ap hi ai) (Ap hj aj) = go_ah inv hi hj >> sequence_ (zipWith (go inv) ai aj)
  
        go_ah _ (Sym i) (Sym j) | i==j = return ()
        go_ah inv (Mu _ _ x) (Mu _ _ y) = go_a inv x y
        go_ah _ _ _ = lift Nothing
        
        tellInv True (x,y) = tell (y,x)
        tellInv False (x,y) = tell (x,y)
