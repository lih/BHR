module Data.Probability where

import Definitive

newtype ProbT t m a = ProbT (WriterT (Product t) (LogicT m) a)
                    deriving (Unit,Functor,SemiApplicative,Applicative
                             ,Semigroup,Monoid
                             ,MonadFix,MonadWriter (Product t))
instance Ring t => Monad (ProbT t m) where join = coerceJoin ProbT
type Prob t a = ProbT t Id a

i'ProbT :: Iso (ProbT t m a) (ProbT t' m' a') (WriterT (Product t) (LogicT m) a) (WriterT (Product t') (LogicT m') a')
i'ProbT = iso ProbT (\(ProbT p) -> p)
probT :: (Monad m,Monad m') => Iso (ProbT t m a) (ProbT t' m' a') (m [(t,a)]) (m' [(t',a')])
probT = listLogic.mapping (i'pair i'_ id).writerT.i'ProbT
prob :: Iso (Prob t a) (Prob t' a') [(t,a)] [(t',a')]
prob = i'Id.probT

c'prob :: Constraint t -> Constraint (Prob t a)
c'prob _ = c'_

instance (Monad m,Invertible t) => MonadList (ProbT t m) where
  fork l = pure [(x,a) | a <- l]^.probT
    where x = recip (size l)

sample :: Monoid t => (a -> Bool) -> Prob t a -> (t,t)
sample px p = foldMap (\(t,x) -> (if px x then t else zero,t)) (p^..prob)
sampleAll :: (Ord b,Monoid t) => (a -> b) -> Prob t a -> Map b t
sampleAll fx p = foldr (\(t,x) -> at (fx x).l'Just zero %~ (+t)) zero (p^..prob)
normalize :: (Monad m,Invertible t) => ProbT t m a -> ProbT t m a
normalize = from probT %~ map (\l -> let sz = recip (foldMap fst l) in map (first (*sz)) l) 
