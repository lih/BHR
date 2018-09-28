{-# LANGUAGE ImplicitParams, RankNTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, LambdaCase, TupleSections, TypeFamilies, TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
module Main where

import Definitive
import Language.Parser
import Algebra.Monad.Concatenative
import System.IO (openFile,hSetBuffering,hGetBuffering,hIsTerminalDevice,BufferMode(..),IOMode(..),hClose)
import System.Environment (getArgs)
import Console.Readline (readline,addHistory,setCompletionEntryFunction)
import System.IO.Unsafe (unsafeInterleaveIO)
import Data.IORef
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))
import Data.List (sortBy)

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

mayLens :: Lens (Maybe b) (Maybe b') a a' -> Lens (Maybe b) (Maybe b') (Maybe a) (Maybe a')
mayLens l = lens (\ma -> ma >>= by l) (\ma mb' -> maybe Nothing (trace "Just" . Just . set l mb') ma)

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
        withEnv env d x m = foldr (\(i,as) ma -> ma <++> (foldl'.foldl') (\l a -> (tell [(env,i,x)] >> return a) <++> l) zero as)
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

showNode = showNode' zero
showNode' :: NodeDir String -> [(String,Node)] -> Node -> String
showNode' dir = go 0
  where go d env x | (pats,k):_ <- findPattern dir x = par (if empty pats then 1000 else 1) d $ intercalate " " $ (k:) $ [
                       go 2 (env'+env) hole
                       | (env',_,hole) <- sortBy (comparing (by l'2)) pats]
        go _ _ (Universe u) = "Set"+show u
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
                  
            go_col' d' recs (Universe u) = do
              envargs <- take d' <$> ask
              let tIH = bind Prod root_args ihRoot
                  ihRoot = Cons (Ap (Sym (nargs-d-1)) [Cons (Ap (Sym (j+nargs)) []) | j <- reverse [0..d-1]])
              return $ Bind Prod xn tIH (Universe (u+1))
            go_col' _ _ _ = zero

takeLast n l = drop (length l-n) l

printStackVal o dir ctx _x = writeHString o . (+"\n") $ case _x of
  StackExtra (Opaque _x) -> case _x of
    COCExpr d e -> -- "<"+show d+">:"+
      showNode' dir (takeLast d ctx) e
    COCNull -> "(null)"
    COCDir d -> show d
  _ -> show _x
data COCBuiltin = COCB_ShowStack | COCB_Show | COCB_Print | COCB_Open | COCB_ExecModule
                | COCB_ToInt | COCB_Concat | COCB_Uni | COCB_Hyp
                | COCB_ShowContext | COCB_Quit | COCB_Var
                | COCB_Ap | COCB_Bind Bool BindType
                | COCB_TypeOf | COCB_Mu
                | COCB_HypBefore | COCB_Subst | COCB_Rename
                | COCB_ContextVars
                | COCB_GetShowDir | COCB_SetShowDir | COCB_InsertNodeDir
                | COCB_Format
                deriving Show

data COCState = COCState {
  _endState :: Bool,
  _context :: [(String,Node)],
  _showDir :: NodeDir String,
  _outputHandle :: Handle
  }
endState :: Lens' COCState Bool
endState = lens _endState (\x y -> x { _endState = y })
context :: Lens' COCState [(String,Node)]
context = lens _context (\x y -> x { _context = y })
showDir :: Lens' COCState (NodeDir String)
showDir = lens _showDir (\x y -> x { _showDir = y })
outputHandle :: Lens' COCState Handle
outputHandle = lens _outputHandle (\x y -> x { _outputHandle = y })

stringWords = fromBlank
  where fromBlank (c:t) | c `elem` " \n\t\r" = fromBlank t
                        | c == '"' = fromQuote id t
                        | otherwise = fromWChar (c:) t
        fromBlank "" = []
        fromQuote k ('"':t) = ('"':k "\""):fromBlank t
        fromQuote k ('\\':c:t) = fromQuote (k.(qChar c:)) t
          where qChar 'n' = '\n' ; qChar 't' = '\t' ; qChar x = x
        fromQuote k (c:t) = fromQuote (k.(c:)) t
        fromQuote k "" = ['"':k "\""]
        fromWChar k (c:t) | c `elem` " \n\t\r" = k "":fromBlank t
                          | otherwise = fromWChar (k.(c:)) t
        fromWChar k "" = [k ""]

runCOCBuiltin COCB_Quit = runExtraState (endState =- True)
runCOCBuiltin COCB_ShowStack = do
  s <- runStackState get
  ex <- runExtraState get
  lift $ for_ (reverse s) $ printStackVal (ex^.outputHandle) (ex^.showDir) (ex^.context)
runCOCBuiltin COCB_ShowContext = do
  ex <- runExtraState get
  lift $ snd $ foldr (\(name,typ) (ctx',pr) -> ((name,typ):ctx',do
                                                   pr
                                                   putStrLn $ name+" : "+showNode' (ex^.showDir) ctx' typ
                                                   )) ([],unit) (ex^.context)
runCOCBuiltin COCB_Show = do
  s <- runStackState get
  ex <- runExtraState get
  lift $ for_ (take 1 s) $ printStackVal (ex^.outputHandle) (ex^.showDir) (ex^.context)
runCOCBuiltin COCB_Print = do
  s <- runStackState get
  o <- runExtraState (getl outputHandle)
  lift $ for_ (take 1 s) $ \case
    StackSymbol s' -> writeHString o s'
    _ -> return ()

runCOCBuiltin COCB_Format = do
  ex <- runExtraState get
  let format ('%':s) (StackSymbol h:t) = first (h+) (format s t)
      format ('%':s) (StackInt h:t) = first (show h+) (format s t)
      format ('%':s) (StackExtra (Opaque (COCExpr d h)):t) = first (showNode' (ex^.showDir) (takeLast d (ex^.context)) h+) (format s t)
      format ('%':s) x = first ("(obj)"+) (format s (drop 1 x))
      format (c:s) t = first (c:) (format s t)
      format "" t = ("",t)
  runStackState $ modify $ \case
    StackSymbol s:t -> uncurry ((:) . StackSymbol) (format s t)
    st -> st

runCOCBuiltin COCB_Open = do
  s <- runStackState get
  case s of
    StackSymbol f:t -> do
      xs <- lift (try (return []) (try (readString f) (readString (f+".md")) >>= maybe undefined return . matches Just literate))
      runStackState (put (StackProg xs:t))
    _ -> return ()
  where literate = intercalate [":\n"] <$> sepBy' (cmdline (single '>') <+? cmdline (several "    ")
                                                            <+? commentline) (single '\n')
        wrapLabel hide x = "<label class=\"hide-label\"><input type=\"checkbox\" class=\"capricon-hide\" checked=\"checked\"/><span class=\"capricon-"+hide+"\"></span><span class=\"capricon-reveal\">"+x+"</span></label>"
        wrapResult x l = (":<div class=\"capricon-"+x+"result\">") : l + [":</div>"]
        cmdline pre = map (\x -> (":"+wrapLabel "hideparagraph" ("<pre class=\"capricon capricon-paragraph\">\n"+intercalate "\n" (map fst x)+"\n</pre>"))
                                 : wrapResult "paragraph" (foldMap snd x)) (sepBy1' go (single '\n'))
          where go = do pre; many' (noneOf "\n") <&> \x -> (x,stringWords x)
        commentline = map (foldMap (pure . (':':) <|> \(x,t) -> t+[':':(wrapLabel "hidestache" $ "<pre class=\"capricon\">"+x+"</pre>")])) $ (<* lookingAt eol)
          $ many' (map Left (many1' (noneOf "{\n" <+? (fill '{' $ single '{' <* lookingAt (noneOf "{"))))
                    <+? map Right (between (several "{{") (several "}}")
                                    (many1' (noneOf "}" <+? fill '}' (single '{' <* lookingAt (noneOf "}"))) <&> \x -> (x,wrapResult "" (stringWords x)))))
                      
                      
runCOCBuiltin COCB_ToInt = runStackState $ modify $ \case
  StackSymbol s:t -> StackInt (read s):t
  st -> st
runCOCBuiltin COCB_Concat = runStackState $ modify $ \case
  StackSymbol s:StackSymbol s':t -> StackSymbol (s'+s):t
  st -> st

runCOCBuiltin COCB_Uni = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackInt n:t -> StackExtra (Opaque (COCExpr (length ctx) (Universe n))):t
    st -> st
runCOCBuiltin COCB_Var = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackSymbol name:t | Just i <- lookup name (zipWith (second . const) [0..] ctx) ->
                         StackExtra (Opaque (COCExpr (length ctx) (Cons (Ap (Sym i) [])))):t
    st -> st
runCOCBuiltin COCB_Ap = do
  ctx <- runExtraState (getl context)
  let adj d dd x = inc_depth (dd+nctx-d) x
      nctx = length ctx
      env = map snd ctx
  runStackState $ modify $ \case
    whole@(StackExtra (Opaque (COCExpr df f)):StackExtra (Opaque (COCExpr dx x)):t) ->
      let x' = adj dx 1 x ; f' = adj df 0 f in
        StackExtra (Opaque (COCExpr nctx (subst f' (Cons (Ap (Sym 0) [x'])) env))):t
    x -> x
runCOCBuiltin (COCB_Bind close bt) = do
  ctx <- runExtraState (getl context) 
  let d = length ctx
      setVal (StackExtra (Opaque (COCExpr d' e')))
        | i <- d-d'
        , d==d' || not close
        , (ctxh,(x,tx):ctxt) <- splitAt i ctx
        = StackExtra (Opaque (COCExpr (d'-1) (Bind bt x tx e')))
      setVal (StackDict dict) = StackDict (map setVal dict)
      setVal (StackList l) = StackList (map setVal l)
      setVal x = x
      setStack (x:t) = setVal x:if close then setStack t else t
      setStack [] = []
                                                         
  ctx' <- runStackState $ id <~ map (,if close && nonempty ctx then tail ctx else ctx) setStack
  runDictState $ modify $ map setVal
  runExtraState (context =- ctx')
runCOCBuiltin COCB_Mu = do
  ctx <- runExtraState (getl context)
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
runCOCBuiltin COCB_TypeOf = do
  ctx <- runExtraState (getl context)
  runStackState $ modify $ \case
    StackExtra (Opaque (COCExpr d (Cons (Ap (Sym i) [])))):t
      | (_,ti):_ <- drop i ctx ->
          StackExtra (Opaque (COCExpr (d-i-1) ti)):t
    StackExtra (Opaque (COCExpr d e)):t -> (:t) $ StackExtra $ Opaque $ case type_of e (takeLast d (map snd ctx)) of
      Just te -> COCExpr d te
      Nothing -> COCNull
    st -> st

runCOCBuiltin COCB_ExecModule = do
  st <- runStackState get
  case st of
    StackSymbol f:StackProg p:t -> do
      old <- runDictState get
      o <- lift $ openFile f WriteMode
      oldH <- runExtraState (outputHandle <~ \x -> (o,x))
      traverse_ (execSymbol runCOCBuiltin runComment) p
      new <- runDictState (id <~ (old,))
      runExtraState (outputHandle =- oldH)
      lift $ hClose o
      runStackState $ put $ StackDict new:t
    _ -> return ()

runCOCBuiltin COCB_Hyp = do
  ass <- runStackState $ id <~ \case
    StackSymbol name:StackExtra (Opaque (COCExpr d typ)):t -> (t,Just (d,(name,typ)))
    st -> (st,Nothing)
  case ass of
    Just (d,x) -> runExtraState $ context =~ \ctx -> (second (inc_depth (length ctx-d)) x:ctx)
    Nothing -> return ()
runCOCBuiltin COCB_HypBefore = do
  ctx <- runExtraState (getl context)
  let csz = length ctx
      adj hi i j = if i+j>=hi then j+1 else j
  ctx' <- runStackState $ id <~ \case
    StackSymbol h:StackSymbol h':StackExtra (Opaque (COCExpr d e)):t
      | (hi,_):_ <- select ((==h) . fst . snd) (zip [0..] ctx)
      , all (>hi+d-csz) (free_vars e) ->
        let ctx' = foldr (\x k i -> case compare hi i of
                             LT -> x:k (i+1)
                             EQ -> second (adjust_depth (adj hi i)) x:(h',inc_depth (csz-(d+hi+1)) e):k (i+1)
                             GT -> second (adjust_depth (adj hi i)) x:k (i+1))
                   (\_ -> []) ctx 0
            adjE x@(StackExtra (Opaque (COCExpr d' e'))) =
              let i = csz-d'
              in if i<=hi then StackExtra (Opaque (COCExpr (d+1) (adjust_depth (adj (hi+1) i) e')))
                 else x
            adjE x = x
        in (map adjE t,ctx')
    st -> (st,ctx)
  runExtraState (context =- ctx')
runCOCBuiltin COCB_Subst = do
  ctx <- runExtraState (getl context)
  let csz = length ctx
  ctx' <- runStackState $ id <~ \case
    StackSymbol h:StackExtra (Opaque (COCExpr d e)):t
      | (hi,_):_ <- select ((==h) . fst . snd) (zip [0..] ctx)
      , all (>hi+d-csz) (free_vars e) ->
        let ctx' = foldr (\x k i env -> case compare i hi of
                             LT -> second (\xv -> substn e (hi-i) xv env) x:k (i+1) (tail env)
                             EQ -> k (i+1) (tail env)
                             GT -> x:k (i+1) (tail env)) (\_ _ -> []) ctx 0 (map snd ctx)
            adjE x@(StackExtra (Opaque (COCExpr d' e'))) =
              let i = csz - d'
              in if i<=hi then StackExtra (Opaque (COCExpr (d-1) ((substn e (hi-i) e' (map snd (drop i ctx))))))
                 else x
            adjE x = x
        in (map adjE t,ctx')
    st -> (st,ctx)
  runExtraState (context =- ctx')
runCOCBuiltin COCB_Rename = do
  ctx <- runExtraState (getl context)
  ctx' <- runStackState $ id <~ \case
    StackSymbol s:StackSymbol s':t -> (t,map (\(n,v) -> (if n==s then s' else n, v)) ctx)
    st -> (st,ctx)
  runExtraState (context =- ctx')
runCOCBuiltin COCB_ContextVars = do
  ctx <- runExtraState (getl context)
  runStackState $ modify (StackList (map (StackSymbol . fst) ctx):)

runCOCBuiltin COCB_GetShowDir = do
  dir <- runExtraState (getl showDir)
  runStackState $ modify $ (StackExtra (Opaque (COCDir (map StackSymbol dir))):)
runCOCBuiltin COCB_SetShowDir = do
  mod' <- runStackState $ id <~ \case
    StackExtra (Opaque (COCDir d)):t -> (t,showDir =- map (\(StackSymbol s) -> s) d)
    st -> (st,return ())
  runExtraState mod'
runCOCBuiltin COCB_InsertNodeDir = do
  runStackState $ modify $ \case
    x:StackExtra (Opaque (COCExpr d e)):StackExtra (Opaque (COCDir dir)):t ->
      StackExtra (Opaque (COCDir (dir&at e %- Just x))):t
    st -> st

data COCValue = COCExpr Int Node | COCNull | COCDir (NodeDir (StackVal String COCBuiltin COCValue))

cocDict = mkDict ((".",StackProg []):("version",StackSymbol "0.6"):
                   [(x,StackBuiltin b) | (x,b) <- [
                       ("io/quit"                 , Builtin_Extra COCB_Quit               ),
                       ("io/show"                 , Builtin_Extra COCB_Show               ),
                       ("io/print"                , Builtin_Extra COCB_Print              ),
                       ("io/stack"                , Builtin_Extra COCB_ShowStack          ),
                       ("io/context"              , Builtin_Extra COCB_ShowContext        ),
                       ("io/open"                 , Builtin_Extra COCB_Open               ),
                       ("io/format"               , Builtin_Extra COCB_Format             ),
                       
                       ("def"                     , Builtin_Def                           ),
                       ("$"                       , Builtin_DeRef                         ),
                       
                       ("arith/++"                , Builtin_Extra COCB_Concat             ),
                       ("arith/+"                 , Builtin_Add                           ),
                       ("arith/-"                 , Builtin_Sub                           ),
                       ("arith/*"                 , Builtin_Mul                           ),
                       ("arith/div"               , Builtin_Div                           ),
                       ("arith/mod"               , Builtin_Mod                           ),
                       ("arith/sign"              , Builtin_Sign                          ),
                       
                       ("exec"                    , Builtin_Exec                          ),
                       ("to-int"                  , Builtin_Extra COCB_ToInt              ),
                       ("clear"                   , Builtin_Clear                         ),
                       ("pop"                     , Builtin_Pop                           ),
                       ("popn"                    , Builtin_PopN                          ),
                       ("dup"                     , Builtin_Dup                           ),
                       ("dupn"                    , Builtin_DupN                          ),
                       ("swap"                    , Builtin_Swap                          ),
                       ("swapn"                   , Builtin_SwapN                         ),
                       ("pick"                    , Builtin_Pick                          ),

                       ("dict/current"            , Builtin_CurrentDict                   ),
                       ("dict/empty"              , Builtin_Empty                         ),
                       ("lookup"                  , Builtin_Lookup                        ),
                       ("dict/insert"             , Builtin_Insert                        ),
                       ("dict/delete"             , Builtin_Delete                        ),
                       ("dict/keys"               , Builtin_Keys                          ),
                       ("dict/module"             , Builtin_Extra COCB_ExecModule         ),

                       ("cocdict/get-show-dict"   , Builtin_Extra COCB_GetShowDir         ),
                       ("cocdict/set-show-dict"   , Builtin_Extra COCB_SetShowDir         ),
                       ("cocdict/insert-show-dict", Builtin_Extra COCB_InsertNodeDir      ),
                       
                       ("["                       , Builtin_ListBegin                     ),
                       ("]"                       , Builtin_ListEnd                       ),
                       ("list/each"               , Builtin_Each                          ),
                       ("list/range"              , Builtin_Range                         ),
                       
                       ("coc/universe"            , Builtin_Extra COCB_Uni                ),
                       ("coc/hypothesis"          , Builtin_Extra COCB_Var                ),
                       ("coc/apply"               , Builtin_Extra COCB_Ap                 ),
                       ("coc/lambda"              , Builtin_Extra (COCB_Bind False Lambda )),
                       ("coc/forall"              , Builtin_Extra (COCB_Bind False Prod   )  ),
                       ("coc/mu"                  , Builtin_Extra COCB_Mu                 ),

                       ("context/intro"           , Builtin_Extra COCB_Hyp                ),
                       ("context/intro-before"    , Builtin_Extra COCB_HypBefore          ),
                       ("context/conclude-lambda" , Builtin_Extra (COCB_Bind True Lambda  ) ),
                       ("context/conclude-forall" , Builtin_Extra (COCB_Bind True Prod    )   ),
                       ("context/rename"          , Builtin_Extra COCB_Rename             ),
                       ("context/subst"           , Builtin_Extra COCB_Subst              ),
                       ("context/type"            , Builtin_Extra COCB_TypeOf             ),
                       ("context/context-vars"    , Builtin_Extra COCB_ContextVars        )
                       ]])
  where mkDict = foldr addElt (c'map zero)
        addElt (x,v) = atP (splitPath x) %- Just v
        splitPath ('/':x) = ("",uncurry (:) (splitPath x))
        splitPath (h:t) = let ~(w,l) = splitPath t in (h:w,l)
        splitPath [] = ("",[])
        atP (h,[]) = at h
        atP (h,x:t) = at h.l'Just (StackDict zero).t'StackDict.atP (x,t)

runComment c = do
  o <- runExtraState (getl outputHandle)
  lift $ writeHString o c

main = do
  isTerm <- hIsTerminalDevice stdin
  libdir <- getXdgDirectory XdgData "capricon"
  symList <- newIORef (keys cocDict)
  let getAll = unsafeInterleaveIO $ do
        ln <- readline "CaPriCon> "
        lns <- getAll
        case ln of
          Just x -> do addHistory x; return $ x + " .\n" + lns
          Nothing -> putStr "\n" >> return ""
  setCompletionEntryFunction $ Just $ \line -> do
    sl <- readIORef symList
    case reverse (words (line+"?")) of
      "?":_ -> return sl
      wp:_ -> let wps = length wp-1; wp' = init wp in return [w | w <- sl, take wps w==wp']
      _ -> return []
  str <- stringWords <$> if isTerm then getAll else readHString stdin
  args <- (foldMap (\x -> [libdir</>x,x]) <$> getArgs) >>= map (words . fold) . traverse (try (return []) . readString)
  execS (foldr (\sym mr -> do
                   execSymbol runCOCBuiltin runComment sym
                   hasQuit <- runExtraState (getl endState)
                   d <- runDictState get
                   lift (writeIORef symList (keys d))
                   unless hasQuit mr
               ) unit (args+str)^..concatT) (defaultState cocDict (COCState False [] zero stdout))
