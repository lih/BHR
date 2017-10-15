{-# LANGUAGE ScopedTypeVariables #-}
module Data.Queue where

import Definitive

data Front
data Back
newtype DeQue a = DeQue ([a],[a])
instance Semigroup (DeQue a) where
  DeQue (h,t) + DeQue (h',t') = DeQue (h+reverse t,t'+reverse h')
deriving instance Monoid (DeQue a)
instance Functor DeQue where
  map f (DeQue (h,t)) = DeQue (map f h,map f t)
instance Foldable DeQue where
  fold (DeQue (h,t)) = fold h + fold t
instance Traversable DeQue where
  sequence (DeQue (fh,ft)) = liftA2 (map2 DeQue (,)) (sequence fh) (reverse<$>sequence (reverse ft))

newtype Queue push pop a = Queue { deque :: DeQue a }
                           deriving (Semigroup,Monoid,Functor,Foldable)
instance Traversable (Queue push pop) where sequence = coerceSequence Queue
c'queue :: Constraint push -> Constraint pop -> Constraint (Queue push pop a)
c'queue _ _ = c'_
c'front :: Constraint Front
c'front = c'_
c'back :: Constraint Back
c'back = c'_

queue :: Queue x y a -> Queue s t a
queue (Queue q) = Queue q

class Direction t where
  isFront :: t -> Bool
instance Direction Front where isFront _ = True
instance Direction Back where isFront _ = False
instance forall push pop a. (Direction push,Direction pop) => Stream a (Queue push pop a) where
  cons = if isFront (undefined :: push) then pushFront else pushBack 
    where
      pushFront a (Queue (DeQue (h,t))) = Queue (DeQue (a:h,t))
      pushBack a (Queue (DeQue (h,t))) = Queue (DeQue (h,a:t))
  uncons = l $ if isFront (undefined :: pop) then popFront else popBack
    where
      l f = f . deque
      popFront (DeQue (a:h,t)) = Just (a,Queue (DeQue (h,t)))
      popFront (DeQue ([],[])) = Nothing
      popFront (DeQue ([],t)) = popFront (DeQue (reverse t,[]))
      popBack (DeQue (h,a:t)) = Just (a,Queue (DeQue (h,t)))
      popBack (DeQue ([],[])) = Nothing
      popBack (DeQue (h,[])) = popBack (DeQue ([],reverse h))
