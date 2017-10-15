module Algebra.Subtractive where

type a:*:b = (a,b)
newtype a:+:b = Union (forall r. (a -> r) :*: (b -> r) -> r)

newtype a:-:b = Diff (forall r. (b -> r) -> a -> r)
type a:/:b = b -> a
