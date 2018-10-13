{-# LANGUAGE UndecidableInstances #-}
module Data.CaPriCon where

import Definitive

data BindType = Lambda | Prod
              deriving (Show,Eq,Ord)
data Node = Bind BindType String Node Node
          | Cons Application
          | Universe Int
          deriving Show
data ApHead = Sym Int | Mu [(String,Node,Node)] [Node] Application
            deriving Show
data Application = Ap ApHead [Node]
                 deriving Show 
type Env = [Node]

instance (Semigroup a, Semigroup (Coforest f a)) => Semigroup (Cofree f a) where
  Step a b + Step a' b' = Step (a+a') (b+b')
instance (Monoid a, Monoid (Coforest f a)) => Monoid (Cofree f a) where
  zero = Step zero zero
instance (DataMap (f (FreeMap f a)) k (FreeMap f a)) => DataMap (FreeMap f a) [k] a where
  at [] = lens (\(Step a _) -> a) (\(Step _ m) a -> Step a m)
  at (h:t) = coforest.at h.l'Just zero.at t
coforest :: Lens' (Cofree f a) (Coforest f a)
coforest = lens (\(Step _ m) -> m) (\(Step a _) m -> Step a m)

type FreeMap f a = Cofree f (Maybe a)

data NodeDir a = NodeDir
  (Map BindType (NodeDir (NodeDir a)))
  (ApDir a)
  (Map Int a)
  deriving (Eq,Ord,Show)
instance Functor NodeDir where
  map f (NodeDir a b c) = NodeDir (map3 f a) (map3 f b) (map f c)
instance Foldable NodeDir where
  fold (NodeDir a b c) = (fold.map fold.map2 fold) a + (fold.map fold.map2 fold) b + fold c
instance Traversable NodeDir where
  sequence (NodeDir a b c) = NodeDir<$>sequence3 a<*>sequence3 b<*>sequence c

sequence2 :: (Applicative f,Traversable t,Traversable u) => t (u (f a)) -> f (t (u a))
sequence2 = sequence.map sequence
sequence3 :: (Applicative f,Traversable t,Traversable u,Traversable v) => t (u (v (f a))) -> f (t (u (v a)))
sequence3 = sequence.map sequence2

i'NodeDir :: Iso (NodeDir a) (NodeDir a')
             ((,,) (Map BindType (NodeDir (NodeDir a)))
               (ApDir a)
               (Map Int a))
             ((,,) (Map BindType (NodeDir (NodeDir a')))
               (ApDir a')
               (Map Int a'))
i'NodeDir = iso (\(x,y,z) -> NodeDir x y z) (\(NodeDir x y z) -> (x,y,z))

type ApDir a = AHDir (FreeMap NodeDir a)
data AHDir a = AHDir
  (Map Int a)
  (Map Int (ApDir a))
  deriving (Eq,Ord,Show)
instance Functor AHDir where
  map f (AHDir a b) = AHDir (map f a) ((map2.map2) f b)
instance Foldable AHDir where
  fold (AHDir a b) = fold a + (fold.map fold.map2 fold.map3 fold) b
instance Traversable AHDir where
  sequence (AHDir a b) = AHDir<$>sequence a<*>(sequence3.map3 sequence) b
i'AHDir :: Iso (AHDir a) (AHDir a')
           ((,) (Map Int a) (Map Int (ApDir a)))
           ((,) (Map Int a') (Map Int (ApDir a')))
i'AHDir = iso (uncurry AHDir) (\(AHDir x y) -> (x,y))

i'Cofree :: Iso (Cofree f a) (Cofree f' a') (a,Coforest f a) (a',Coforest f' a')
i'Cofree = iso (uncurry Step) (\(Step x y) -> (x,y))

instance Semigroup (NodeDir a) where NodeDir a b c + NodeDir a' b' c' = NodeDir (a+a') (b+b') (c+c')
instance Monoid (NodeDir a) where zero = NodeDir zero zero zero
instance DataMap (NodeDir a) Node a where
  at (Bind t _ tx e) = from i'NodeDir.l'1.at t.l'Just zero.at tx.l'Just zero.at e
  at (Cons a) = from i'NodeDir.l'2.atAp a
  at (Universe u) = from i'NodeDir.l'3.at u

instance Semigroup (AHDir a) where AHDir a b + AHDir a' b' = AHDir (a+a') (b+b')
instance Monoid (AHDir a) where zero = AHDir zero zero
instance DataMap (AHDir a) ApHead a where
  at (Sym i) = from i'AHDir.l'1.at i
  at (Mu xs _ a) = from i'AHDir.l'2.at (length xs).l'Just zero.atAp a

  
atAp :: Application -> Lens' (ApDir a) (Maybe a)
atAp (Ap h xs) = at h.l'Just zero.at xs


mayChoose (Just x) = return x
mayChoose Nothing = zero

(<++>) :: WriterT w [] a -> WriterT w [] a -> WriterT w [] a
a <++> b = a & from writerT %~ (+ b^..writerT)

findPattern :: NodeDir a -> Node -> [([([(String,Node)],Int,Node)],a)]
findPattern = \x y -> go [] x y^..writerT
  where go :: [(String,Node)] -> NodeDir a -> Node -> WriterT [([(String,Node)],Int,Node)] [] a
        go_a :: [(String,Node)] -> ApDir a -> Application -> WriterT [([(String,Node)],Int,Node)] [] a
        go_ah :: [(String,Node)] -> AHDir a -> ApHead -> WriterT [([(String,Node)],Int,Node)] [] a
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
free_vars :: Node -> Set Int
free_vars (Bind _ _ tx e) = free_vars tx + delete (-1) (map (subtract 1) (free_vars e))
free_vars (Cons a) = freeA a
  where freeA (Ap (Sym i) xs) = singleton' i + foldMap free_vars xs
        freeA (Ap (Mu env _ a') xs) = foldMap free_vars xs +
          map (subtract envS) (freeA a' - fromKList [0..envS-1])
          where envS = length env
free_vars _ = zero

subst :: MonadReader Env m => Node -> Node -> m Node
subst = flip substn 0
substn :: MonadReader Env m => Node -> Int -> Node -> m Node
substn val n | n>=0 = go n
             | otherwise = error "'subst' should not be called with a negative index"
  where go d (Bind t x tx e) = do
          tx' <- go d tx
          Bind t x tx' <$> local (tx':) (go (d+1) e)
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
                                     , subst x (Cons (Ap (Mu [] muEnv (Ap (Sym 0) [])) [Cons (Ap (Sym j) []) | j <- reverse [1..envS]]))]
                                else [return x]
                               | x <- xs])
              return $ foldl' (\e (x,_,tx) -> Bind Lambda x tx e) a' env
        go_mu _ e t (Cons a) = return $ Cons (Ap (Mu e t a) [])
        go_mu _ _ _ x' = error $ "Cannot produce an induction principle for a term : "+show x'

        rec_subst (y:t) (Bind Lambda _ _ e) = rec_subst t =<< subst y e
        rec_subst xs (Cons (Ap h hxs)) = return (Cons (Ap h (hxs+xs)))
        rec_subst [] x = return x
        rec_subst _ x = error $ "Invalid substitution of non-lambda expression : "+show x

par lvl d msg | d>lvl = "("+msg+")"
              | otherwise = msg
fresh env v = head $ select (not . (`elem` env)) (v:[v+show i | i <- [0..]])
freshContext = go []
  where go env ((n,v):t) = let n' = fresh env n in (n',(n,v)):go (n':env) t
        go _ [] = []

type StringPattern = [String :+: Int]

showNode = showNode' zero
showNode' :: NodeDir ([String],StringPattern) -> [(String,Node)] -> Node -> String
showNode' dir = go 0
  where go d env x | (pats,(_,k)):_ <- findPattern dir x =
                       let holes = c'map $ fromAList [(i,(env',hole)) | (env',i,hole) <- pats] in
                         par (if empty pats then 1000 else 1) d $ intercalate " " [
                         case word of
                           Left w -> w
                           Right i | Just (env',hole) <- lookup i holes -> go 2 (env'+env) hole
                                   | otherwise -> ""
                         | word <- k]
        go _ _ (Universe u) = "Set"+show u
        go d env whole@(Bind t aname atype body) | t == Lambda || 0`isKeyIn`free_vars body = par 0 d $ bind_head t + drop 1 (bind_tail env whole)
                                                 | otherwise = par 0 d $ go 1 env atype + " -> " + go 0 ((aname,atype):env) body
          where bind_head Lambda = "λ"
                bind_head Prod = "∀"
                bind_tail env' (Bind t' x tx e) | t==t' && (t==Lambda || 0`isKeyIn`free_vars e) = " ("+x'+":"+go 0 env' tx+")"+bind_tail ((x',tx):env') e
                  where x' = fresh (map fst env') x
                bind_tail env' x = ", "+go 0 env' x
        go d env (Cons a) = showA d a
          where showA _ (Ap h xs) =
                  let ni = case h of
                             Sym i -> case drop i env of
                                        (h',_):_ -> h'
                                        _ -> "#"+show i
                             Mu _ _ a' -> "μ("+showA 0 a'+")"
                      lvl = if empty xs then 1000 else 1
                  in par lvl d $ ni+foldMap ((" "+) . go 2 env) xs

type_of :: MonadReader Env m => Node -> m (Maybe Node)
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
                  rec_subst subs =<< subst (Cons a') preret
                      
                rec_subst (y:t) (Bind Prod _ _ e) = rec_subst t =<< subst y e
                rec_subst [] x = return x
                rec_subst _ _ = zero

mu_type :: MonadReader Env m => Node -> m (Maybe Node)
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
                  let args = select (not . (`isKeyIn`recs)) [0..d'-1]
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
