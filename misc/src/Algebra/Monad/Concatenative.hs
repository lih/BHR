{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving #-}
module Algebra.Monad.Concatenative(ConcatT,concatT,MonadStack(..),StackBuiltin(..),StackVal(..),StackState,defaultState,Opaque(..)) where

import Definitive

newtype Opaque a = Opaque a
instance Show (Opaque a) where show _ = "#<opaque>"
data StackBuiltin b = Builtin_ListBegin | Builtin_ListEnd
                    | Builtin_Clear | Builtin_Pop | Builtin_Dup | Builtin_Swap | Builtin_SwapN
                    | Builtin_If | Builtin_Each
                    | Builtin_DeRef | Builtin_Def
                    | Builtin_Exec
                    | Builtin_Extra b
                    deriving Show
data StackVal s b a = StackBuiltin (StackBuiltin b)
                    | StackInt Int
                    | StackSymbol s
                    | StackList [StackVal s b a]
                    | StackProg [s]
                    | StackExtra (Opaque a)
                    deriving Show

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

data AtomClass s = OpenBrace | CloseBrace | Quoted s | Other s
class Ord s => StackSymbol s where atomClass :: s -> AtomClass s
instance StackSymbol String where
  atomClass "{" = OpenBrace
  atomClass "}" = CloseBrace
  atomClass ('\'':t) = Quoted t
  atomClass x = Other x

execSymbolImpl :: (StackSymbol s, MonadState (StackState st s b a) m) => (StackBuiltin b -> m ()) -> s -> m ()
execSymbolImpl execBuiltin' atom = do
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
    _ -> case st^.depth of
           0 -> case st^.dict.at atom of
             Just v -> exec v
             Nothing -> stack =~ (StackSymbol atom:)
           _ -> progStack =~ (atom:)
  where exec (StackBuiltin b) = execBuiltin' b
        exec (StackProg p) = traverse_ (execSymbolImpl execBuiltin') p
        exec x = stack =~ (x:)

execBuiltin :: (StackSymbol s, MonadState (StackState st s b a) m) => (b -> m ()) -> StackBuiltin b -> m ()
execBuiltin runExtra = go
  where 
    go Builtin_Def = get >>= \st -> case st^.stack of
      (val:StackSymbol var:tl) -> do dict =~ insert var val ; stack =- tl
      _ -> return ()
    go Builtin_ListBegin = stack =~ (StackBuiltin Builtin_ListBegin:)
    go Builtin_ListEnd = stack =~ \st -> let (h,_:t) = break (\x -> case x of
                                                                               StackBuiltin Builtin_ListBegin -> True
                                                                               _ -> False) st
                                                  in StackList (reverse h):t
    go Builtin_Clear = stack =- []
    go Builtin_Pop = stack =~ drop 1
    go Builtin_Swap = stack =~ \st -> case st of x:y:t -> y:x:t ; _ -> st
    go Builtin_SwapN = stack =~ \st -> case st of
      StackInt n:st' ->
        case splitAt (n+1) st' of
          (x:tx,y:ty) -> y:tx+(x:ty)
          _ -> st
      _ -> st
    go Builtin_Dup = stack =~ \st -> case st of x:t -> x:x:t ; _ -> st
    go Builtin_Each = do
      st <- get
      case st^.stack of
        e:StackList l:t -> do
          stack =- t
          for_ l $ \x -> do stack =~ (e:) . (x:) ; go Builtin_Exec
        _ -> return ()
    go Builtin_DeRef = do
      st <- get
      stack =~ \x -> case x of
                       StackSymbol v:t -> maybe (StackSymbol v) id (st^.dict.at v):t
                       _ -> x
    go Builtin_Exec = do
      st <- get
      case st^.stack of
        StackProg p:t -> do stack =- t ; traverse_ (execSymbolImpl go) p
        StackBuiltin b:t -> do stack =- t ; go b
        _ -> return ()
    go Builtin_If = stack =~ \st -> case st of
      _:y:StackInt 0:t -> y:t
      x:_:_:t -> x:t
      _ -> st
    go (Builtin_Extra x) = runExtra x


class (StackSymbol s,Monad m) => MonadStack st s b a m | m -> st s b a where
  execSymbol :: (b -> m ()) -> s -> m ()
  runStackState :: State [StackVal s b a] x -> m x
  runExtraState :: State st x -> m x

newtype ConcatT st b o s m a = ConcatT { _concatT :: StateT (StackState st s b o) m a }
                          deriving (Functor,SemiApplicative,Unit,Applicative,MonadIO,MonadTrans)
instance Monad m => Monad (ConcatT st b o s m) where join = coerceJoin ConcatT
instance (StackSymbol s,Monad m) => MonadStack st s b a (ConcatT st b a s m) where
  execSymbol x y = ConcatT $ execSymbolImpl (execBuiltin (map _concatT x)) y
  runStackState st = ConcatT $ (\x -> return (swap $ stack (map swap (st^..state)) x))^.stateT
  runExtraState st = ConcatT $ (\x -> return (swap $ extraState (map swap (st^..state)) x))^.stateT

defaultState = StackState [] [] 0

concatT :: Iso (ConcatT st b o s m a) (ConcatT st' b' o' s' m' a') (StateT (StackState st s b o) m a) (StateT (StackState st' s' b' o') m' a')
concatT = iso ConcatT (\(ConcatT x) -> x)
