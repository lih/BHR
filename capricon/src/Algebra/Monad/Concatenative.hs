{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, LambdaCase #-}
module Algebra.Monad.Concatenative(ConcatT,concatT,MonadStack(..),StackBuiltin(..),StackVal(..),t'StackDict,StackState,defaultState,Opaque(..)) where

import Definitive
import Language.Parser

newtype Opaque a = Opaque a
instance Show (Opaque a) where show _ = "#<opaque>"
data StackBuiltin b = Builtin_ListBegin | Builtin_ListEnd
                    | Builtin_Clear | Builtin_Stack
                    | Builtin_Pick 
                    | Builtin_Pop  | Builtin_PopN
                    | Builtin_Dup  | Builtin_DupN
                    | Builtin_Swap | Builtin_SwapN
                    | Builtin_Range | Builtin_Each
                    | Builtin_Add | Builtin_Sub | Builtin_Mul | Builtin_Div | Builtin_Mod | Builtin_Sign
                    | Builtin_DeRef | Builtin_Def
                    | Builtin_Exec
                    | Builtin_CurrentDict | Builtin_Empty | Builtin_Insert | Builtin_Lookup | Builtin_Delete | Builtin_Keys
                    | Builtin_Extra b
                    deriving Show
data StackVal s b a = StackBuiltin (StackBuiltin b)
                    | StackInt Int
                    | StackSymbol s
                    | StackList [StackVal s b a]
                    | StackDict (Map s (StackVal s b a))
                    | StackProg [s]
                    | StackExtra (Opaque a)
                    deriving Show

t'StackDict :: Traversal' (StackVal s b a) (Map s (StackVal s b a))
t'StackDict k (StackDict d) = StackDict <$> k d
t'StackDict _ x = return x

data StackState st s b a = StackState {
  _stack :: [StackVal s b a],
  _progStack :: [s],
  _depth :: Int,
  _dict :: Map s (StackVal s b a),
  _extraState :: st
  }

stack :: Lens' (StackState st s b a) [StackVal s b a]
stack = lens _stack (\x y -> x { _stack = y })
progStack :: Lens' (StackState st s b a) [s]
progStack = lens _progStack (\x y -> x { _progStack = y })
depth :: Lens' (StackState st s b a) Int
depth = lens _depth (\x y -> x { _depth = y })
dict :: Lens' (StackState st s b a) (Map s (StackVal s b a))
dict = lens _dict (\x y -> x { _dict = y })
extraState :: Lens st st' (StackState st s b a) (StackState st' s b a)
extraState = lens _extraState (\x y -> x { _extraState = y })

data AtomClass s = OpenBrace | CloseBrace | Number Int | Quoted s | Comment s | Other s
class Ord s => StackSymbol s where atomClass :: s -> AtomClass s
instance StackSymbol String where
  atomClass "{" = OpenBrace
  atomClass "}" = CloseBrace
  atomClass ('\'':t) = Quoted t
  atomClass ('"':t) = Quoted (init t)
  atomClass (':':t) = Comment t
  atomClass x = maybe (Other x) Number (matches Just readable x)

execSymbolImpl :: (StackSymbol s, MonadState (StackState st s b a) m) => (StackBuiltin b -> m ()) -> (s -> m ()) -> s -> m ()
execSymbolImpl execBuiltin' onComment atom = do
  st <- get
  case atomClass atom of
    OpenBrace -> do depth =~ (+1) ; when (st^.depth > 0) (progStack =~ (atom:))
    CloseBrace -> do
      depth =~ subtract 1
      if st^.depth == 1 then do
        stack =~ (StackProg (reverse $ st^.progStack):)
        progStack =- []
        else progStack =~ (atom:)
    Quoted a | st^.depth==0 -> stack =~ (StackSymbol a:)
    Comment a -> onComment a
    Number n | st^.depth==0 -> stack =~ (StackInt n:)
    _ -> case st^.depth of
           0 -> case st^.dict.at atom of
             Just v -> exec v
             Nothing -> stack =~ (StackSymbol atom:)
           _ -> progStack =~ (atom:)
  where exec (StackBuiltin b) = execBuiltin' b
        exec (StackProg p) = traverse_ (execSymbolImpl execBuiltin' onComment) p
        exec x = stack =~ (x:)

execBuiltin :: (StackSymbol s, MonadState (StackState st s b a) m) => (b -> m ()) -> (s -> m ()) -> StackBuiltin b -> m ()
execBuiltin runExtra onComment = go
  where 
    go Builtin_Def = get >>= \st -> case st^.stack of
      (val:StackSymbol var:tl) -> do dict =~ insert var val ; stack =- tl
      _ -> return ()
    go Builtin_ListBegin = stack =~ (StackBuiltin Builtin_ListBegin:)
    go Builtin_ListEnd = stack =~ \st -> let (h,_:t) = break (\x -> case x of
                                                                               StackBuiltin Builtin_ListBegin -> True
                                                                               _ -> False) st
                                                  in StackList (reverse h):t
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
    go Builtin_Dup = stack =~ \st -> case st of x:t -> x:x:t ; _ -> st
    go Builtin_DupN = stack =~ \st -> case st of StackInt n:t | (h,x:t') <- splitAt n t -> (x:h)+(x:t') ; _ -> st
    go Builtin_Range = stack =~ \st -> case st of StackInt n:t -> StackList [StackInt i | i <- [0..n-1]]:t ; _ -> st
    go Builtin_Each = do
      st <- get
      case st^.stack of
        e:StackList l:t -> do
          stack =- t
          for_ l $ \x -> do stack =~ (e:) . (x:) ; go Builtin_Exec
        _ -> return ()

    go Builtin_CurrentDict = getl dict >>= \d -> stack =~ (StackDict d:)
    go Builtin_Empty = stack =~ (StackDict zero:)
    go Builtin_Insert = stack =~ \case
      x:StackSymbol s:StackDict d:t -> StackDict (insert s x d):t
      st -> st
    go Builtin_Delete = stack =~ \case
      StackSymbol s:StackDict d:t -> StackDict (delete s d):t
      st -> st
    go Builtin_Lookup = stack =~ \case
      StackSymbol s:StackDict d:t -> case lookup s d of
        Just x -> StackSymbol s:x:t
        Nothing -> StackDict d:t
      st -> st
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
        StackProg p:t -> do stack =- t ; traverse_ (execSymbolImpl go onComment) p
        StackBuiltin b:t -> do stack =- t ; go b
        _ -> return ()
    go (Builtin_Extra x) = runExtra x


class (StackSymbol s,Monad m) => MonadStack st s b a m | m -> st s b a where
  execSymbol :: (b -> m ()) -> (s -> m ()) -> s -> m ()
  runStackState :: State [StackVal s b a] x -> m x
  runExtraState :: State st x -> m x
  runDictState :: State (Map s (StackVal s b a)) x -> m x

newtype ConcatT st b o s m a = ConcatT { _concatT :: StateT (StackState st s b o) m a }
                          deriving (Functor,SemiApplicative,Unit,Applicative,MonadIO,MonadTrans)
instance Monad m => Monad (ConcatT st b o s m) where join = coerceJoin ConcatT
instance (StackSymbol s,Monad m) => MonadStack st s b a (ConcatT st b a s m) where
  execSymbol x y z = ConcatT $ execSymbolImpl (execBuiltin (map _concatT x) (map _concatT y)) (map _concatT y) z
  runStackState st = ConcatT $ (\x -> return (swap $ stack (map swap (st^..state)) x))^.stateT
  runExtraState st = ConcatT $ (\x -> return (swap $ extraState (map swap (st^..state)) x))^.stateT
  runDictState st = ConcatT $ (\x -> return (swap $ dict (map swap (st^..state)) x))^.stateT

defaultState = StackState [] [] 0

concatT :: Iso (ConcatT st b o s m a) (ConcatT st' b' o' s' m' a') (StateT (StackState st s b o) m a) (StateT (StackState st' s' b' o') m' a')
concatT = iso ConcatT (\(ConcatT x) -> x)
