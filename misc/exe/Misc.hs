{-# LANGUAGE ImplicitParams, RankNTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where

import Definitive

data BindType = Lam | Prod
              deriving (Eq,Show)
data ApType = Ap Int | Delta Node
            deriving (Show)
data Node = Bind BindType String Node Node
          | Shape ApType [Node]
          | Uni Int
          deriving Show

par lvl d msg | d > lvl = "("+msg+")"
              | otherwise = msg

show_node = go 0 []
  where go d env (Bind Prod x tx e) | not (0`isKeyIn`freevars e) = par 0 d $ go 1 env tx + " -> " + go 0 (fresh env x:env) e
        go d env (Bind t x tx e) = par 0 d $ binder_prefix t+binder_body t env x tx e
          where binder_prefix Lam = "λ"
                binder_prefix Prod = "∀"
        go d env (Shape (Ap i) args) = par lvl d $ intercalate " " (ni:map (go 2 env) args)
          where lvl = if empty args then 1000 else 1 :: Int
                ni = case drop i env of
                       [] -> "#"+show i
                       n:_ -> n
        go d env (Shape (Delta v) args) = par 1 d $ "δ" + intercalate " " (map (go 2 env) (v:args))
        go _ _ (Uni i) = "Set"+show i
        fresh env x = head [y | y <- x:[x+show i | i <- [0..]], not (y`elem`env)]
        binder_body t env x tx e = "("+x'+":"+go 0 env tx+")"+binder_tail t (x':env) e
          where x' = fresh env x
        binder_tail t env (Bind t' x tx e) | t==t' && (t'/=Prod || 0`isKeyIn`freevars e) = " "+binder_body t env x tx e
        binder_tail _ env x = ", "+go 0 env x
type InEnv t = (?env :: [String]) => t
infixr 1 -=>, -->
(-=>) :: InEnv ((String,Node) -> (InEnv Node) -> Node)
(x,tx) -=> e = Bind Lam x tx (let ?env = x : ?env in e)
(-->) :: InEnv ((String,Node) -> (InEnv Node) -> Node)
(x,tx) --> e = Bind Prod x tx (let ?env = x : ?env in e)
(-$) :: Node -> Node -> Node
Shape (Ap i) args -$ b = Shape (Ap i) (args+[b])
_ -$ _ = error "Not implemented"

var :: String -> InEnv Node
var s = case [i | (i,s') <- zip [0..] ?env, s==s'] of
  i:_ -> Shape (Ap i) []
  _ -> error $ "No variable named "+s+" in environment"

adjust_vars_depth delta = go 0
  where go d (Bind t x tx e) = Bind t x (go d tx) (go (d+1) e)
        go d (Shape (Ap i) args) = Shape (Ap (if i<d then i else d+delta (i-d))) (map (go d) args)
        go d (Shape (Delta v) args) = Shape (Delta (go d v)) (map (go d) args)
        go _ (Uni u) = Uni u

currentDepth :: MonadReader [Node] m => m Int
currentDepth = length <$> ask

hypotheses :: MonadReader [Node] m => m [Node]
hypotheses = ask <&> zipWith (\i h -> adjust_vars_depth (+i) h) [1..]

hypothesis :: MonadReader [Node] m => Int -> m Node
hypothesis h = head . drop h <$> hypotheses

freevars :: Node -> Set Int
freevars (Bind _ _ tx e) = freevars tx + map (subtract 1) (delete 0 (freevars e))
freevars (Shape (Ap i) args) = touch i (foldMap freevars args)
freevars (Shape (Delta v) args) = freevars v + foldMap freevars args
freevars _ = zero

subst :: MonadReader [Node] m => Node -> Node -> m Node
subst v node = do
  d0 <- currentDepth
  let go (Bind t x tx e) = Bind t x <$> go tx <*> local (tx:) (go e)
      go (Shape (Ap 0) args) = do
        d <- subtract d0 <$> currentDepth
        foldl' (\mf x -> do
                   f <- mf
                   tx' <- type_of x
                   case f of
                     Bind Lam _ tx e -> subst x e
                     _ -> error $ "Invalid application of non-product value:"+show f)
          (pure (adjust_vars_depth (+d) v)) args
      go (Shape (Ap i) args) = do
        d <- subtract d0 <$> currentDepth
        Shape (Ap (if i<d then i else i-1)) <$> traverse go args
      go (Shape (Delta d) args) = liftA2 (\x y -> Shape (Delta x) y) (go d) (traverse go args)
      go (Uni u) = return $ Uni u

  go node

type_of :: MonadReader [Node] m => Node -> m Node
type_of = go
  where go (Bind Lam x tx e) = Bind Prod x tx <$> (local (tx:) $ go e)
        go (Bind Prod _ tx e) = do
          Uni u <- go tx
          Uni u' <- local (tx:) (go e)
          return $ Uni (max u u')
        go (Shape (Ap f) xs) =
          foldl' (\mtf x -> do
                     tf <- mtf
                     tx' <- go x
                     case tf of
                       Bind Prod _ tx e | nonempty (convertible tx tx') -> subst x e
                       _ -> error $ "Invalid type for application:"+show tf)
          (hypothesis f) xs
        go (Uni u) = pure $ Uni (u+1)
        go x = ask >>= \env -> error $ "Cannot infer type for term: "+show (env,x)

convertible (Bind t x tx e) (Bind t' _ tx' e') | t==t' = Bind t x <$> convertible tx' tx <*> convertible e e'
convertible (Shape (Ap i) args) (Shape (Ap i') args') | i==i' = Shape (Ap i) <$> sequence (zipWith convertible args args')
convertible (Uni u) (Uni u') | u<=u' = return $ Uni u'
convertible _ _ = Nothing

drop_last n l = take (length l-n) l

instance MonadReader r m => MonadReader r (ListT m) where
  ask = (pure <$> ask)^.listT
  local f ma = ma & warp (from listT) (local f)

list_patterns :: MonadReader [Node] m => Node -> ListT m ([Bool],Node)
list_patterns node = do
  d0 <- currentDepth
  let candidate p (Bind _ _ _ e) = candidate (p+1) e
      candidate p (Shape (Ap p') _) = p==p'
      candidate _ _ = False
      go stack (Bind Prod x tx e) = do
        (parms,e') <- local (tx:) $ go stack e
        return (parms,Bind Lam x tx e')
      go stack (Shape (Ap p) _) = do
        envsize <- subtract d0 <$> currentDepth
        env <- drop_last d0 <$> ask
        (i,hypi) <- choose . reverse . zip [0..] =<< hypotheses
        guard (candidate p hypi)
        let isrec d t = any (\x -> x-d<envsize && x>=d) (freevars t)
            newparams d (Bind Prod x tx e) | isrec d tx = (True,x,foldl' (\tx' p' -> Bind Prod x p' tx') tx env) : newparams (d+1) e
                                           | otherwise = (False,x,adjust_vars_depth (\i' -> if i'>=d then i'-envsize else i') tx) : newparams (d+1) e
            newparams _ _ = []
        let parms = newparams 0 hypi
        return (parms,Shape (Ap i) [if isR then Shape (Ap j) [Shape (Ap k) [] | k <- reverse [0..envsize-1]]
                                    else Shape (Ap j) []
                                   | (j,(isR,_,_)) <- reverse $ zip [envsize..] (reverse parms)])
      go _ _ = zero
  (parms,node') <- go [] (adjust_vars_depth (+1000) node)
  let np = length parms
      adj i = adjust_vars_depth (\x -> if x+i>=np then subtract (i+1000-np) x else x)
      node'' = foldl' (\e (i,(_,x,tx)) -> Bind Lam x (adj i tx) e) (adj 0 node') (zip [1..] parms)
  return (map (by l'1) parms,node'')

coc_identity = let ?env = [] in ("A",Uni 0) -=> ("x", var "A") -=> var "x"
coc_nat = let ?env = ["B"] in ("A",Uni 0) --> ("x",var "A") --> ("f", ("y",var "A") --> var "A")
                              --> var "A"

main = do
  print coc_identity
  putStrLn (show_node coc_identity)
  putStrLn (show_node (type_of coc_identity []))
  putStrLn (show_node (type_of (type_of coc_identity []) []))
  putStrLn (show_node coc_nat)
  sequence_ [putStrLn (show i+": "+show rs+": "+show_node pat)
            | (i,(rs,pat)) <- zip [0..] $ (list_patterns coc_nat^..listT) [Uni 0]]
