{-# LANGUAGE DeriveGeneric #-}
module Data.CaPriCon.Extraction where

import Definitive
import Data.CaPriCon
import Language.Format
import GHC.Generics (Generic)


data Algebraic str = AFun str (AType str) (Algebraic str)
                   | AApply (Algebraic str) (Algebraic str)
                   | AVar Int
                   deriving (Generic)
data AType str = AArr (AType str) (AType str) | ATVar Int | AAny
               deriving (Show,Generic)

instance Show str => Show (Algebraic str) where
  show = go []
    where go env (AFun x tx e) = "fun ("+show x+" : "+go_t tx+") => "+go (x:env) e
          go env (AApply f x) = "("+go env f+") ("+go env x+")"
          go env (AVar n) | v:_ <- drop n env = show v
                          | otherwise = "__var_"+show n
          go_t (AArr a b) = go_t a + " -> " + go_t b
          go_t (ATVar n) = "'a"+show n
          go_t AAny = "__"

instance Serializable bytes str => Serializable bytes (Algebraic str)
instance Serializable bytes str => Serializable bytes (AType str)
instance Format bytes str => Format bytes (Algebraic str)
instance Format bytes str => Format bytes (AType str)

fromNode :: (IsCapriconString str,MonadReader ([Bool],Env str) m) => Node str -> m (Algebraic str)
fromNode (Bind Lambda x tx e) = do
  let isT = isTypeType tx
  e' <- local ((not isT:)<#>((x,tx):)) (fromNode e)
  if isT then return e'
    else AFun x <$> fromTypeNode tx <*> pure e'
fromNode (Cons a) = fromApplication a
fromNode _ = error "Cannot produce a type-term in a language without first-class types"

fromApplication :: (IsCapriconString str, MonadReader ([Bool],Env str) m) => Application str -> m (Algebraic str)
fromApplication (Ap ah args) = do
  (varKinds,env) <- ask
  let concreteArgs = [arg | (arg,Just t) <- map (\x -> (x,(checkType x^..maybeT) env)) args
                          , not (isTypeType t)]
  case ah of
    Sym s -> foldl' (liftA2 AApply) (pure $ AVar $ sum [if isV then 1 else 0 | isV <- take s varKinds]) (map fromNode concreteArgs)
    Mu _ _ a -> foldl' (liftA2 AApply) (fromApplication a) (map fromNode concreteArgs)
  
fromTypeNode :: MonadReader ([Bool],Env str) m => Node str -> m (AType str)
fromTypeNode (Bind Prod x tx e) = do
  let isT = isTypeType tx
  e' <- local ((not isT:)<#>((x,tx):)) (fromTypeNode e)
  if isT then return e'
    else AArr <$> fromTypeNode tx <*> pure e' 
fromTypeNode (Cons (Ap (Sym s) [])) = do
  (varKinds,_) <- ask
  pure $ ATVar $ sum [if isV then 0 else 1 | isV <- take s varKinds]
fromTypeNode _ = pure AAny

isTypeType :: Node str -> Bool
isTypeType (Universe _) = True
isTypeType (Bind Prod _ _ e) = isTypeType e
isTypeType _ = False

