{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, LambdaCase, DeriveGeneric #-}
module Algebra.Monad.Concatenative(
  -- * Extensible stack types
  StackBuiltin(..),StackSymbol(..),StackVal(..),StackStep(..),StackClosure(..),
  t'StackDict,
  -- * The MonadStack class
  StackState,defaultState,
  MonadStack(..),
  AtomClass(..),
  -- ** A concrete implementation
  ConcatT,concatT,Opaque(..)) where

import Definitive
import Language.Parser
import GHC.Generics (Generic)

newtype Opaque a = Opaque a
                 deriving (Generic)
instance Show (Opaque a) where show _ = "#<opaque>"
data StackStep s b a = VerbStep s | ConstStep (StackVal s b a) | CommentStep s | ClosureStep Bool (StackClosure s b a)
                     deriving (Show,Generic)
data StackClosure s b a = StackClosure [(StackProgram s b a,StackClosure s b a)] (StackProgram s b a)
                        deriving (Show,Generic)
type StackProgram s b a = [StackStep s b a]

i'StackClosure :: Iso' ([(StackProgram s b a,StackClosure s b a)],StackProgram s b a) (StackClosure s b a)
i'StackClosure = iso (\(cs,c) -> StackClosure cs c) (\(StackClosure cs c) -> (cs,c))

t'ClosureStep :: Traversal' (StackStep s b a) (StackClosure s b a)
t'ClosureStep k (ClosureStep b c) = ClosureStep b<$>k c
t'ClosureStep _ x = pure x

allSteps :: Fold' (StackClosure s b a) (StackStep s b a)
allSteps = from i'StackClosure.(l'1.each.l'1.each .+ l'2.each)
subClosure :: Int -> Fold' (StackClosure s b a) (StackClosure s b a)
subClosure 0 = id
subClosure n = (allSteps.t'ClosureStep.subClosure (n+1))
               .+ (from i'StackClosure.l'1.each.l'2.subClosure (n-1))

closureSplices :: Fold' (StackClosure s b a) (StackClosure s b a)
closureSplices = allSteps.t'ClosureStep.subClosure (1::Int)
               
runClosure execBuiltin' onComment clos = do
  p <- flatten =<< forl closureSplices clos (\c -> StackClosure [] <$> flatten c)
  stack =~ (StackProg p:)
  
  where flatten (StackClosure cs c) = do
          pref <- map fold $ for cs $ \(i,StackClosure _ p) -> (i+) <$> do
            traverse_ (runStep execBuiltin' onComment) p
            stack <~ \(h:t) -> (t,[ConstStep h])
          return (pref + c)
          
runStep execBuiltin' onComment (VerbStep s) = getl (dict.at s) >>= \case
  Just v -> runVal v
  Nothing -> stack =~ (StackSymbol s:)
  where runVal (StackBuiltin b) = execBuiltin' b
        runVal (StackProg p) = traverse_ (runStep execBuiltin' onComment) p
        runVal x = stack =~ (x:)
runStep _ _ (ConstStep v) = stack =~ (v:)
runStep _ onComment (CommentStep c) = onComment c
runStep _ _ (ClosureStep True (StackClosure _ p)) = stack =~ (StackProg p:)
runStep execBuiltin' onComment (ClosureStep _ c) = runClosure execBuiltin' onComment c

data StackBuiltin b = Builtin_ListBegin | Builtin_ListEnd
                    | Builtin_Clear | Builtin_Stack
                    | Builtin_Pick | Builtin_Shift | Builtin_Shaft
                    | Builtin_Pop  | Builtin_PopN
                    | Builtin_Dup  | Builtin_DupN
                    | Builtin_Swap | Builtin_SwapN
                    | Builtin_Range | Builtin_Each
                    | Builtin_Add | Builtin_Sub | Builtin_Mul | Builtin_Div | Builtin_Mod | Builtin_Sign
                    | Builtin_DeRef | Builtin_Def
                    | Builtin_Exec
                    | Builtin_CurrentDict | Builtin_Empty | Builtin_Insert | Builtin_Lookup | Builtin_Delete | Builtin_Keys
                    | Builtin_Quote
                    | Builtin_Extra b
                    deriving (Show,Generic)
data StackVal s b a = StackBuiltin (StackBuiltin b)
                    | StackInt Int
                    | StackSymbol s
                    | StackList [StackVal s b a]
                    | StackDict (Map s (StackVal s b a))
                    | StackProg (StackProgram s b a)
                    | StackExtra (Opaque a)
                    deriving (Show,Generic)

t'StackDict :: Traversal' (StackVal s b a) (Map s (StackVal s b a))
t'StackDict k (StackDict d) = StackDict <$> k d
t'StackDict _ x = return x

data StackState st s b a = StackState {
  _stack :: [StackVal s b a],
  _progStack :: [StackClosure s b a],
  _dict :: Map s (StackVal s b a),
  _extraState :: st
  }
  deriving Generic

stack :: Lens' (StackState st s b a) [StackVal s b a]
stack = lens _stack (\x y -> x { _stack = y })
progStack :: Lens' (StackState st s b a) [StackClosure s b a]
progStack = lens _progStack (\x y -> x { _progStack = y })
dict :: Lens' (StackState st s b a) (Map s (StackVal s b a))
dict = lens _dict (\x y -> x { _dict = y })
extraState :: Lens st st' (StackState st s b a) (StackState st' s b a)
extraState = lens _extraState (\x y -> x { _extraState = y })

data AtomClass s = OpenBrace | CloseBrace | OpenSplice | CloseSplice | Number Int | Quoted s | Comment s | Other s
class Ord s => StackSymbol s where atomClass :: s -> AtomClass s
instance StackSymbol String where
  atomClass "{" = OpenBrace
  atomClass "{@" = OpenSplice
  atomClass "}" = CloseBrace
  atomClass "@}" = CloseSplice
  atomClass ('\'':t) = Quoted t
  atomClass ('"':t) = Quoted (init t)
  atomClass (':':t) = Comment t
  atomClass x = maybe (Other x) Number (matches Just readable x)

execSymbolImpl :: (StackSymbol s, MonadState (StackState st s b a) m) => (StackBuiltin b -> m ()) -> (s -> m ()) -> s -> m ()
execSymbolImpl execBuiltin' onComment atom = do
  st <- get
  case (atomClass atom,st^.progStack) of
    (OpenBrace,_) -> progStack =~ (StackClosure [] []:)

    (OpenSplice,StackClosure cs p:ps) ->
      progStack =- StackClosure [] []:StackClosure ((p,StackClosure [] []):cs) []:ps
    (CloseSplice,StackClosure cs p:StackClosure cs' p':ps) ->
      progStack =- StackClosure (set (t'1.l'2) (StackClosure (reverse cs) (reverse p)) cs') p':ps

    (CloseBrace,StackClosure cs p:ps) -> do
      progStack =- ps
      let c = StackClosure (reverse cs) (reverse p)
      execStep ps (ClosureStep (not $ has closureSplices c) c)
    (CloseBrace,[]) -> unit
    (OpenSplice,[]) -> unit
    (CloseSplice,_) -> unit

    (Quoted a,ps) -> execStep ps (ConstStep (StackSymbol a))
    (Comment a,ps) -> execStep ps (CommentStep a)
    (Number n,ps) -> execStep ps (ConstStep (StackInt n))
    (Other s,ps) -> execStep ps (VerbStep s)
  where execStep [] stp = runStep execBuiltin' onComment stp
        execStep (StackClosure cs p:ps) stp = progStack =- (StackClosure cs (stp:p):ps)

execBuiltin :: (StackSymbol s, MonadState (StackState st s b a) m) => (b -> m ()) -> (s -> m ()) -> StackBuiltin b -> m ()
execBuiltin runExtra onComment = go
  where 
    go Builtin_Def = get >>= \st -> case st^.stack of
      (val:StackSymbol var:tl) -> do dict =~ insert var val ; stack =- tl
      _ -> return ()
    go Builtin_ListBegin = stack =~ (StackBuiltin Builtin_ListBegin:)
    go Builtin_ListEnd = stack =~ \st -> let ex acc (StackBuiltin Builtin_ListBegin:t) = (acc,t)
                                             ex acc (h:t) = ex (h:acc) t
                                             ex acc [] = (acc,[])
                                         in let (h,t) = ex [] st in StackList h:t
    go Builtin_Stack = stack =~ \x -> StackList x:x
    go Builtin_Clear = stack =- []
    go Builtin_Pick = stack =~ \st -> case st of StackInt i:StackInt n:t | i<n, x:t' <- drop i t -> x:drop (n-i-1) t'
                                                 _ -> st
    go Builtin_Pop = stack =~ drop 1
    go Builtin_PopN = stack =~ \st -> case st of StackInt n:t | (h,_:t') <- splitAt n t -> h+t' ; _ -> st
    go Builtin_Swap = stack =~ \st -> case st of x:y:t -> y:x:t ; _ -> st
    go Builtin_SwapN = stack =~ \st -> case st of
      StackInt n:st' ->
        case splitAt (n+1) st' of
          (x:tx,y:ty) -> y:tx+(x:ty)
          _ -> st
      _ -> st
    go Builtin_Shift = stack =~ \case
      StackInt n:st' | (h,v:t) <- splitAt n st' -> v:(h+t)
      st -> st
    go Builtin_Shaft = stack =~ \case
      StackInt n:v:st' | (h,t) <- splitAt n st' -> h+(v:t)
      st -> st
    go Builtin_Dup = stack =~ \st -> case st of x:t -> x:x:t ; _ -> st
    go Builtin_DupN = stack =~ \st -> case st of StackInt n:t | x:_ <- drop n t -> x:t ; _ -> st
    go Builtin_Range = stack =~ \st -> case st of StackInt n:t -> StackList [StackInt i | i <- [0..n-1]]:t ; _ -> st
    go Builtin_Each = do
      st <- get
      case st^.stack of
        e:StackList l:t -> do
          stack =- t
          for_ l $ \x -> do stack =~ (x:) ; execVal e
        _ -> return ()

    go Builtin_CurrentDict = getl dict >>= \d -> stack =~ (StackDict d:)
    go Builtin_Empty = stack =~ (StackDict zero:)
    go Builtin_Insert = stack =~ \case
      x:StackSymbol s:StackDict d:t -> StackDict (insert s x d):t
      st -> st
    go Builtin_Delete = stack =~ \case
      StackSymbol s:StackDict d:t -> StackDict (delete s d):t
      st -> st
    go Builtin_Lookup = join $ do
      stack <~ \case
        el:th:StackSymbol s:StackDict d:t -> case lookup s d of
          Just x -> (x:t,execVal th)
          Nothing -> (t,execVal el)
        st -> (st,return ())
    go Builtin_Keys = stack =~ \case
      StackDict d:t -> StackList (map StackSymbol (keys d)):t
      st -> st
    
    go Builtin_Add = stack =~ \st -> case st of StackInt m:StackInt n:t -> StackInt (n+m):t; _ -> st
    go Builtin_Sub = stack =~ \st -> case st of StackInt m:StackInt n:t -> StackInt (n-m):t; _ -> st
    go Builtin_Mul = stack =~ \st -> case st of StackInt m:StackInt n:t -> StackInt (n*m):t; _ -> st
    go Builtin_Div = stack =~ \st -> case st of StackInt m:StackInt n:t -> StackInt (n`div`m):t; _ -> st
    go Builtin_Mod = stack =~ \st -> case st of StackInt m:StackInt n:t -> StackInt (n`mod`m):t; _ -> st
    go Builtin_Sign = stack =~ \st -> case st of StackInt n:t -> StackInt (case compare n 0 of
                                                                              LT -> -1
                                                                              GT -> 1
                                                                              EQ -> 0):t; _ -> st

    go Builtin_DeRef = do
      st <- get
      stack =~ \x -> case x of
                       StackSymbol v:t -> maybe (StackSymbol v) id (st^.dict.at v):t
                       _ -> x
    go Builtin_Exec = do
      st <- get
      case st^.stack of
        StackProg p:t -> do stack =- t ; execVal (StackProg p)
        StackBuiltin p:t -> do stack =- t ; execVal (StackBuiltin p)
        _ -> return ()
    go Builtin_Quote = stack =~ \case
      StackList l:t -> StackProg (map ConstStep l):t
      st -> st
      
    go (Builtin_Extra x) = runExtra x

    execVal (StackProg p) = traverse_ (runStep go onComment) p
    execVal (StackBuiltin b) = go b
    execVal _ = return ()

class (StackSymbol s,Monad m) => MonadStack st s b a m | m -> st s b a where
  execSymbol :: (b -> m ()) -> (s -> m ()) -> s -> m ()
  execProgram :: (b -> m ()) -> (s -> m ()) -> StackProgram s b a -> m ()
  runStackState :: State [StackVal s b a] x -> m x
  runExtraState :: State st x -> m x
  runDictState :: State (Map s (StackVal s b a)) x -> m x

newtype ConcatT st b o s m a = ConcatT { _concatT :: StateT (StackState st s b o) m a }
                          deriving (Functor,SemiApplicative,Unit,Applicative,MonadTrans)
instance Monad m => Monad (ConcatT st b o s m) where join = coerceJoin ConcatT
instance (StackSymbol s,Monad m) => MonadStack st s b a (ConcatT st b a s m) where
  execSymbol x y z = ConcatT $ execSymbolImpl (execBuiltin (map _concatT x) (map _concatT y)) (map _concatT y) z
  execProgram x y p = ConcatT $ traverse_ (runStep (execBuiltin (map _concatT x) (map _concatT y)) (map _concatT y)) p
  runStackState st = ConcatT $ (\x -> return (swap $ stack (map swap (st^..state)) x))^.stateT
  runExtraState st = ConcatT $ (\x -> return (swap $ extraState (map swap (st^..state)) x))^.stateT
  runDictState st = ConcatT $ (\x -> return (swap $ dict (map swap (st^..state)) x))^.stateT

defaultState = StackState [] []

concatT :: Iso (ConcatT st b o s m a) (ConcatT st' b' o' s' m' a') (StateT (StackState st s b o) m a) (StateT (StackState st' s' b' o') m' a')
concatT = iso ConcatT (\(ConcatT x) -> x)
