{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

module Divisible where

import Chess
import Reducible

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Maybe
import Data.Void

instance Reducible a => Reducible [a] where
  reductions xs = reduceList xs ++ reduceListInner xs

-- | All of the single-element deletions of a list.
reduceList :: [a] -> [[a]]
reduceList xs =
  let g as [] = []
      g as (b:bs) = (as ++ bs) : g (as ++ [b]) bs
  in g [] xs

-- | For each element x in the list, for each x' in reductions x, the original list with x replaced with x'.
reduceListInner :: Reducible a => [a] -> [[a]]
reduceListInner xs =
  let g as [] = []
      g as (b:bs) = [as ++ [b'] ++ bs | b' <- reductions b ] ++ g (as ++ [b]) bs
  in g [] xs
;
-- minimize'Counterexample :: Reducible a => (a -> Bool) -> (a -> a)
-- minimize'Counterexample f x = getOp $ minimize'Helper f x

minimize'Helper :: (Divisible f, Reducible a) => a -> f a
minimize'Helper x = case reductions x of
  [] -> conquer
  (x:xs) -> divide (const (x, xs)) (minimize'Helper x) (minimize'Helper xs)

splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)

-- A Counterexample a is either:
-- 1. Nothing: A proof that all children match the predicate
-- 2. Just b: A way of finding a smaller counterexample that fails
newtype Cx b a = Cx { unCx :: a -> Maybe b }

instance Contravariant (Cx b) where
  contramap f (Cx g) = Cx (g . f)

instance Divisible (Cx b) where
  divide split (Cx left) (Cx right) = Cx $ \x ->
    let (lProblem, rProblem) = split x
    -- One branch must produce a counterexample
    in case (left lProblem, right rProblem)  of
      (Just x, _) -> Just x
      (_, Just x) -> Just x
      (Nothing, Nothing) -> Nothing
  conquer = Cx $ const Nothing

instance Decidable (Cx b) where
  lose f = Cx $ const Nothing
  choose split left right = Cx $ \x ->
    -- Both branches must produce a counterexample
    case split x of
      Left l -> unCx left l
      Right r -> unCx right r

minimizeContra :: PerftTest -> Maybe PerftTest
minimizeContra x = unCx (minD (Cx $ liftPred checkBug)) x
  where
    liftPred :: (a -> Bool) -> (a -> Maybe a)
    liftPred pred = \x ->
      if pred x
      then Just x
      else Nothing
                               
      
minD :: forall a f. (Decidable f, Reducible a) => f a -> f a
minD pred = choose splitReductions pred solveList
  where
    splitReductions :: a -> Either a [a]
    splitReductions x = case reductions x of
      [] -> Left x
      xs -> Right xs

    solveList :: f [a]
    solveList = choose splitList caseEmpty caseNonempty
    splitList :: [a] -> Either () (a, [a])
    splitList [] = Left ()
    splitList (x:xs) = Right (x, xs)
    caseEmpty :: f ()
    caseEmpty = conquer
    caseNonempty :: f (a, [a])
    caseNonempty = divided (minD pred) solveList
  
  -- case reductions x of
  -- [] -> conquer
  -- (y:ys) -> divide (splitHead . reductions) (y >$ pred) (contramap _ _ :: f [a])
  --   chooseList :: (Decidable f) => [a] -> Either () (a, [a])
  --   chooseList xs = case xs of
  --     [] -> Left ()
  --     (y:ys) -> Right (y, ys)
    -- divideList :: Divisible f => (a -> [b]) -> [f b] -> f a
    -- divideList x = undefined
    -- --divideList :: (Divisible f) => a -> [f a]
    -- divideList x = map (\x -> x >$ pred) $ reductions x
      -- [] -> conquer
      -- (y:ys) -> divide splitHead (minD pred y) _
  -- [] -> conquer
  -- (x:xs) -> divide (splitHead . reductions) (x >$ pred) (divideList xs)
  -- where
  --   divideList :: (Divisible f) => f a
  --   divideList = _

  -- where
  --   decideElement x =
  --     if pred x
  --     then Left x
  --     else Right ()
  -- divide (splitHead . reductions) x (reductions x)

-- minimize'Helper2 :: (Divisible f, Reducible a) => a -> a
-- minimize'Helper2 g x = case reductions x of
--   [] -> conquer
--   (y:ys) -> divide (splitHead . reductions) conquer conquer
  -- [] -> conquer
  -- (x:xs) -> divide (splitHead . reductions) (minimize'Helper2 x) (minimize'Helper2 xs)

-- -- | c = counterexample type
-- data Counterexample c where
--   Term :: Reducible c => Maybe c -> Counterexample c

-- instance Semigroup Counterexample where
--   (<>) (Term a) (Term b) =
--     Term $ case (a, b) of
--     (Nothing, Nothing) -> Nothing
--     (Nothing, Just x)  -> Just x
--     (Just x, Nothing)  -> Just x
--     -- Note: We only need one counterexample, so choose the left
--     (Just x, Just y)   -> Just x 

-- instance Monoid Counterexample where
--   mempty = Term mempty

-- instance Divisible Counterexample where
--   divide split left right =
--     case 
--   conquer = undefined

-- instance Decidable Counterexample where
--   lose _ = Nothing
--   choose split left right =
--     case (term left, term right) of
--       (Nothing, Nothing) -> lose
--       (Nothing, Just x) -> term $ Just x
--       (Just x, Nothing) -> term $ Just x
--       (Just x, Just y) -> term $ Just x

-- funzip :: Functor f => f (a, b) -> (f a, f b)
-- funzip = fmap fst &&& fmap snd

-- instance Divisible m => Divisible (ListT m) where
--   divide f (ListT l) (ListT r) = ListT $ divide (funzip . map f) l r
--   conquer = ListT conquer

-- f :: a -> (a,c)
-- l,r :: m a



-- ListT $ divide (funzip . map f) l r 
