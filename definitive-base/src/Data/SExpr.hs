module Data.SExpr where

import Definitive

data SExpr a = Group [SExpr a]
             | Symbol a

i'SExpr :: Iso (SExpr a) (SExpr b) (a:+:[SExpr a]) (b:+:[SExpr b])
i'SExpr = iso f f'
  where f = Symbol<|>Group
        f' (Symbol a) = Left a ; f' (Group g) = Right g

child :: Traversal (SExpr a) (SExpr a) (SExpr a) (SExpr a)
child = from i'SExpr.t'2.traverse
