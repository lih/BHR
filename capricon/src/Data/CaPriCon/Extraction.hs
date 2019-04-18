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

par lvl d msg | d>lvl = "("+msg+")"
              | otherwise = msg

instance IsCapriconString str => Show (Algebraic str) where
  show = go 0 ([],[])
    where go d env (AFun x tx e) = par 0 d $ "fun ("+toString x+" : "+go_t 0 tx+") => "+go 0 (first (x:) env) e
          go d env (AApply f x) = par 1 d $ go 1 env f+" "+go 2 env x
          go _ env (AVar n) | v:_ <- drop n (fst env) = toString v
                            | otherwise = "__var_"+show n
          go_t d (AArr a b) = par 0 d $ go_t 1 a + " -> " + go_t 0 b
          go_t _ (ATVar n) = "'a"+show n
          go_t _ AAny = "__"

instance Serializable bytes str => Serializable bytes (Algebraic str)
instance Serializable bytes str => Serializable bytes (AType str)
instance Format bytes str => Format bytes (Algebraic str)
instance Format bytes str => Format bytes (AType str)

fromTerm :: (Show ax,IsCapriconString str,MonadReader ([Bool],Env str (Term str ax)) m) => Term str ax -> m (Algebraic str)
fromTerm (Bind Lambda x tx e) = do
  let isT = isTypeType tx
  e' <- local ((not isT:)<#>((x,tx):)) (fromTerm e)
  if isT then return e'
    else AFun x <$> fromTypeTerm tx <*> pure e'
fromTerm (Cons a) = fromApplication a
fromTerm _ = error "Cannot produce a type-term in a language without first-class types"

fromApplication :: (Show ax,IsCapriconString str, MonadReader ([Bool],Env str (Term str ax)) m) => Application str ax -> m (Algebraic str)
fromApplication (Ap ah args) = do
  (varKinds,env) <- ask
  let concreteArgs = [arg | (arg,Just t) <- map (\x -> (x,(checkType x^..maybeT) env)) args
                          , not (isTypeType t)]
  case ah of
    Sym s -> foldl' (liftA2 AApply) (pure $ AVar $ sum [if isV then 1 else 0 | isV <- take s varKinds]) (map fromTerm concreteArgs)
    Mu _ _ a -> foldl' (liftA2 AApply) (fromApplication a) (map fromTerm concreteArgs)
    Axiom _ _ -> undefined
  
fromTypeTerm :: MonadReader ([Bool],Env str (Term str ax)) m => Term str ax -> m (AType str)
fromTypeTerm (Bind Prod x tx e) = do
  let isT = isTypeType tx
  e' <- local ((not isT:)<#>((x,tx):)) (fromTypeTerm e)
  if isT then return AAny
    else AArr <$> fromTypeTerm tx <*> pure e' 
fromTypeTerm (Cons (Ap (Sym s) [])) = do
  (varKinds,_) <- ask
  pure $ ATVar $ sum [if isV then 0 else 1 | isV <- take s varKinds]
fromTypeTerm _ = pure AAny

isTypeType :: Term str ax -> Bool
isTypeType (Universe _) = True
isTypeType (Bind Prod _ _ e) = isTypeType e
isTypeType _ = False

