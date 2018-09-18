{-# LANGUAGE ImplicitParams, RankNTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, LambdaCase, TupleSections #-}
module Main where

import Definitive
import Language.Parser
import Algebra.Monad.Concatenative
import System.IO (hSetBuffering,hGetBuffering,BufferMode(..))
import System.Environment (getArgs)

data BindType = Lambda | Prod
              deriving (Show,Eq)
data Node = Bind BindType String Node Node
          | Cons Application
          | Universe Int
          deriving Show
data ApHead = Sym Int | Mu [(String,Node,Node)] [Node] Application
            deriving Show
data Application = Ap ApHead [Node]
                 deriving Show 
type Env = [Node]

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
        go d (Universe u) = Universe u
        go d (Cons a) = Cons (go_a d a)
        go_a d (Ap (Sym i) subs) | i<d = Ap (Sym i) (map (go d) subs)
                                 | otherwise = Ap (Sym (d+f (i-d))) (map (go d) subs)
        go_a d (Ap (Mu env t a') subs) = Ap (Mu
                                            (reverse $ zipWith (\i (x,tx,tx') -> (x,go (d+i) tx,go (d+i) tx')) [0..] (reverse env))
                                            (zipWith (\i -> go (d+length env+i)) [0..] t)
                                            (go_a (d+length env) a')) (map (go d) subs)
inc_depth 0 = \x -> x
inc_depth dx = adjust_depth (+dx)
free_vars :: Node -> Set Int
free_vars (Bind _ _ tx e) = free_vars tx + delete (-1) (map (subtract 1) (free_vars e))
free_vars (Cons a) = freeA a
  where freeA (Ap (Sym i) xs) = singleton' i + foldMap free_vars xs
        freeA (Ap (Mu env _ a') xs) = foldMap free_vars xs +
          map (subtract envS) (freeA a' - fromKList [0..envS-1])
          where envS = length env
free_vars _ = zero

subst :: MonadReader Env m => Node -> Node -> m Node
subst val = go 0
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
          x <- go_a d a
          xs' <- traverse (go d) xs
          case x of
            Cons a' -> return (Cons (Ap (Mu e t a') xs'))
            _ -> rec_subst xs' =<< go_mu d e t x
  
        go_mu d env (tx':t) (Bind Lambda x tx e) = go_mu d ((x,tx,tx'):env) t e
        go_mu d env _ (Cons (Ap (Sym i) xs))
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
showNode :: [(String,Node)] -> Node -> String
showNode = go 0
  where go _ _ (Universe u) = "Set"+show u
        go d env whole@(Bind t aname atype body) | t == Lambda || 0`isKeyIn`free_vars body = par 0 d $ bind_head t + drop 1 (bind_tail env whole)
                                                 | otherwise = par 0 d $ go 1 env atype + " -> " + go 0 ((aname,atype):env) body
          where bind_head Lambda = "λ"
                bind_head Prod = "∀"
                bind_tail env' (Bind t' x tx e) | t==t' && (t==Lambda || 0`isKeyIn`free_vars e) = " ("+x'+":"+go 0 env' tx+")"+bind_tail ((x',tx):env') e
                  where x' = fresh (map fst env') x
                bind_tail env' x = ", "+go 0 env' x
        go d env (Cons a) = showA d a
          where showA d' (Ap h xs) =
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
                  ta <- local (map (by l'2) env+) (go' a')
                  preret <- maybeT $^ mu_type $ foldl' (\e (x,tx,_) -> Bind Prod x tx e) ta env
                  rec_subst subs =<< subst (Cons a') preret
                      
                rec_subst (y:t) (Bind Prod _ _ e) = rec_subst t =<< subst y e
                rec_subst [] x = return x
                rec_subst _ x = zero

mu_type :: MonadReader Env m => Node -> m (Maybe Node)
mu_type root_type = yb maybeT $ go 0 root_type
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
    go d (Cons (Ap (Sym i) args)) = return $ Cons (Ap (Sym i) $ args + [Cons (Ap (Sym nargs) [])])
    go _ _ = zero

    go_col d xn = go_col' 0 (c'set zero)
      where go_col' d' recs (Bind Prod x tx@(Cons (Ap (Sym i) subs)) e)
              | constr_ind d d' i = do
                  let tx' = bind Prod root_args (adjust_depth (\i' -> if constr_ind d d' i' then (i'-d')+(nargs-d) else i'+nargs) tx)
                      tIx = Cons $ Ap (Sym (i+1)) $ map (inc_depth 1) subs + [Cons (Ap (Sym 0) [])]
                  e' <- local ((tx:) . (undefined:)) (go_col' (d'+2) (touch (1 :: Int) (map (+2) recs))
                                                      (adjust_depth (\j -> if j==0 then j else j+1) e))
                  return $ Bind Prod x tx' (Bind Prod x tIx e')
            go_col' d' recs (Bind Prod x tx e) = Bind Prod x tx <$> local (tx:) (go_col' (d'+1) (map (+1) recs) e)
            go_col' d' recs (Cons a@(Ap (Sym i) xs))
              | constr_ind d d' i = do
                  let args = select (not . (`isKeyIn`recs)) [0..d'-1]
                      lastE = bind Lambda root_args (Cons (Ap (Sym (nargs-d-1))
                                                           [Cons (Ap (Sym (j'+nargs)) args')
                                                           | j <- args
                                                           , let (j',args') | (j+1)`isKeyIn`recs = (j+1,[Cons (Ap (Sym k) []) | k <- reverse [0..nargs-1]])
                                                                            | otherwise = (j,[])
                                                           ]))
                  return $ Cons (Ap (Sym i) $ xs+[lastE])
                  
            go_col' d' recs u@(Universe _) = do
              envargs <- take d' <$> ask
              let tIH = bind Prod root_args ihRoot
                  ihRoot = Cons (Ap (Sym (nargs-d-1)) [Cons (Ap (Sym (j+nargs)) []) | j <- reverse [0..d-1]])
              return $ Bind Prod xn tIH u
            go_col' _ _ _ = zero

takeLast n l = drop (length l-n) l

printStackVal ctx _x = putStrLn $ case _x of
  StackExtra (Opaque _x) -> case _x of
    COCExpr d e -> showNode (takeLast d ctx) e
    COCNull -> "(null)"
  _ -> show _x
data COCBuiltin = COCB_ShowStack | COCB_Show | COCB_Open | COCB_ToInt | COCB_Uni | COCB_Hyp | COCB_Context | COCB_Quit | COCB_Var | COCB_Ap | COCB_Bind BindType | COCB_TypeOf | COCB_MuType | COCB_Mu
                deriving Show
runCOCBuiltin COCB_ShowStack = do
  s <- runStackState get
  (_,ctx) <- runExtraState get
  lift $ for_ (reverse s) $ printStackVal ctx
runCOCBuiltin COCB_Show = do
  s <- runStackState get
  (_,ctx) <- runExtraState get
  lift $ for_ (take 1 s) $ printStackVal ctx
runCOCBuiltin COCB_Open = do
  s <- runStackState get
  case s of
    StackSymbol f:t -> do
      xs <- lift (try (return []) (words <$> readString f))
      runStackState (put (StackProg xs:t))
    _ -> return ()
runCOCBuiltin COCB_ToInt = runStackState $ modify $ \case
  StackSymbol s:t -> StackInt (read s):t
  st -> st
runCOCBuiltin COCB_Uni = runStackState $ modify $ \case
  StackInt n:t -> StackExtra (Opaque (COCExpr 0 (Universe n))):t
  st -> st
runCOCBuiltin COCB_Hyp = do
  ass <- runStackState $ id <~ \case
    StackSymbol name:StackExtra (Opaque (COCExpr d typ)):t -> (t,Just (d,(name,typ)))
    st -> (st,Nothing)
  case ass of
    Just (d,x) -> runExtraState $ l'2 =~ \ctx -> (second (inc_depth (length ctx-d)) x:ctx)
    Nothing -> return ()
runCOCBuiltin COCB_Quit = runExtraState (l'1 =- True)
runCOCBuiltin COCB_Context = do
  (_,ctx) <- runExtraState get
  lift $ snd $ foldr (\(name,typ) (ctx',pr) -> ((name,typ):ctx',do
                                                   pr
                                                   putStrLn $ name+" : "+showNode ctx' typ
                                                   )) ([],unit) ctx
runCOCBuiltin COCB_Var = do
  (_,ctx) <- runExtraState get
  runStackState $ modify $ \case
    StackSymbol name:t | Just i <- lookup name (zipWith (second . const) [0..] ctx) ->
                         StackExtra (Opaque (COCExpr (length ctx) (Cons (Ap (Sym i) [])))):t
    st -> st
runCOCBuiltin COCB_Ap = do
  (_,ctx) <- runExtraState get
  let adj d dd x = inc_depth (dd+nctx-d) x
      nctx = length ctx
      env = map snd ctx
  runStackState $ modify $ \case
    whole@(StackExtra (Opaque (COCExpr df f)):StackExtra (Opaque (COCExpr dx x)):t) ->
      let x' = adj dx 1 x ; f' = adj df 0 f in
        StackExtra (Opaque (COCExpr nctx (subst f' (Cons (Ap (Sym 0) [x'])) env))):t
    x -> x
runCOCBuiltin (COCB_Bind bt) = do
  mx <- runExtraState $ l'2 <~ \case
    x:t -> (t,Just (x,1+length t))
    x -> (x,Nothing)
  case mx of
    Just ((x,tx),d) -> runStackState $ each =~ \case
      StackExtra (Opaque (COCExpr d' e)) | d==d' -> StackExtra (Opaque (COCExpr (d'-1) (Bind bt x tx e)))
      y -> y
    Nothing -> return ()
runCOCBuiltin COCB_TypeOf = do
  (_,ctx) <- runExtraState get
  runStackState $ modify $ \case
    StackExtra (Opaque (COCExpr d e)):t -> (:t) $ StackExtra $ Opaque $ case type_of e (takeLast d (map snd ctx)) of
      Just te -> COCExpr d te
      Nothing -> COCNull
    st -> st
runCOCBuiltin COCB_MuType = do
  (_,ctx) <- runExtraState get
  runStackState $ modify $ \case
    StackExtra (Opaque (COCExpr d e)):t -> (:t) $ StackExtra $ Opaque $ case mu_type e (takeLast d (map snd ctx)) of
      Just te -> COCExpr d te
      Nothing -> COCNull
    st -> st
runCOCBuiltin COCB_Mu = do
  (_,ctx) <- runExtraState get
  let locEnv d = map snd (takeLast d ctx)
  runStackState $ modify $ \case
    StackExtra (Opaque (COCExpr d e)):t -> 
      case type_of e (locEnv d) >>= \te -> mu_type te (locEnv d) of
        Just mte -> let args (Bind Prod _ tx e') = tx:args e'
                        args _ = []
                    in (:t) $ StackExtra $ Opaque $ COCExpr d $
                       subst e (Cons (Ap (Mu [] (args mte) (Ap (Sym 0) [])) [])) (locEnv d)
        Nothing -> StackExtra (Opaque COCNull):t
    st -> st
data COCValue = COCExpr Int Node | COCNull

cocDict = fromAList (("null",StackExtra (Opaque COCNull)):
                      [(x,StackBuiltin b) | (x,b) <- [
                          ("quit"    , Builtin_Extra COCB_Quit      ),
                          ("show"    , Builtin_Extra COCB_Show      ),
                          ("stack"   , Builtin_Extra COCB_ShowStack ),
                          ("open"    , Builtin_Extra COCB_Open      ),
                          ("int"     , Builtin_Extra COCB_ToInt     ),

                          ("def"     , Builtin_Def                  ),
                          ("$"       , Builtin_DeRef                ),
                          ("dup"     , Builtin_Dup                  ),
                          ("clear"   , Builtin_Clear                ),
                          ("["       , Builtin_ListBegin            ),
                          ("]"       , Builtin_ListEnd              ),
                          ("exec"    , Builtin_Exec                 ),
                          ("each"    , Builtin_Each                 ),
                          ("pop"     , Builtin_Pop                  ),
                          ("swap"    , Builtin_Swap                 ),
                          ("swapn"   , Builtin_Swap                 ),
                          
                          ("context" , Builtin_Extra COCB_Context   ),
                          ("Set"     , Builtin_Extra COCB_Uni       ),
                          ("hyp"     , Builtin_Extra COCB_Hyp       ),
                          ("var"     , Builtin_Extra COCB_Var       ),
                          ("ap"      , Builtin_Extra COCB_Ap        ),
                          ("lambda"  , Builtin_Extra (COCB_Bind Lambda)),
                          ("forall"  , Builtin_Extra (COCB_Bind Prod)),
                          ("type"    , Builtin_Extra COCB_TypeOf    ),
                          ("mu-type" , Builtin_Extra COCB_MuType    ),
                          ("mu"      , Builtin_Extra COCB_Mu        )
                              ]])

main = do
  str <- words <$> getContents
  args <- getArgs
  execS (foldr (\sym mr -> do
                   execSymbol runCOCBuiltin sym
                   hasQuit <- runExtraState (gets fst)
                   unless hasQuit mr
               ) unit (args+str)^..concatT) (defaultState cocDict (False,[]))
