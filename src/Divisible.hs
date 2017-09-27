{-# LANGUAGE GADTs, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, RankNTypes, LambdaCase #-}

module Divisible where

import Chess
import Reducible

import Control.Arrow
import Data.Profunctor
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Maybe
import Data.Void
import Data.List (uncons)
import Unsafe.Coerce

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

-- minimize'Counterexample :: Reducible a => (a -> Bool) -> (a -> a)
-- minimize'Counterexample f x = getOp $ minimize'Helper f x

minimize'Helper :: (Divisible f, Reducible a) => a -> f a
minimize'Helper x = case reductions x of
  [] -> conquer
  (x:xs) -> divide (const (x, xs)) (minimize'Helper x) (minimize'Helper xs)

splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)

-- A Counterexample a is: 
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

instance Refinable (Cx b) where
  implies l r = Cx $ \x ->
    case unCx l x of
      Nothing -> Nothing
      Just _ -> unCx r x

minimizeD'' :: PerftTest -> Maybe PerftTest
minimizeD'' x = unCx (minimizeD (Cx $ liftPred checkBug)) x
  where
    liftPred :: (a -> Bool) -> (a -> Maybe a)
    liftPred pred = \x ->
      if pred x
      then Just x
      else Nothing

newtype ProofChain b a = ProofChain { unChain :: a -> (Maybe b, [b]) }


instance Contravariant (ProofChain b) where
  contramap f (ProofChain g) = ProofChain $ g . f

instance Divisible (ProofChain b) where
  divide split (ProofChain l) (ProofChain r) = ProofChain $ \x ->
    let (left, right) = split x
    in case (l left, r right) of
      ((Just x', xs), _) -> (Just x', x' : xs)
      (_, (Just x', xs)) -> (Just x', x' : xs)
      ((Nothing,_), (Nothing,_)) -> (Nothing, [])
  conquer = ProofChain $ const (Nothing, [])

instance Decidable (ProofChain b) where
  lose _ = conquer
  choose split left right = ProofChain $ \x ->
    case split x of
      Left l -> unChain left l
      Right r -> unChain right r

instance Refinable (ProofChain b) where
  implies l r = ProofChain $ \x ->
    case unChain l x of
      l'@(Nothing, _) -> l'
      (Just _, ls) ->
        case unChain r x of
          r'@(Nothing, _) -> r'
          (Just r', rs) -> (Just r', ls ++ rs)
      

minimizeChain :: PerftTest -> (Maybe PerftTest, [PerftTest])
minimizeChain x = unChain (minimizeD (ProofChain $ liftPred checkBug)) x
  where
    liftPred :: (a -> Bool) -> (a -> (Maybe a, [a]))
    liftPred pred = \x ->
      if pred x
      then (Just x, [])
      else (Nothing, [])

-- myMap :: (f a -> f [a]) -> (f a -> f [a])
-- myMap 

-- 1. Split x into its reductions
-- 2. Find a failing reduction
-- 3. Recurse into the failing reduction
-- 4. Otherwise, return the original

findD :: Decidable f => f a -> f [a]
findD p = chooseMaybe uncons conquer (divided p (findD p))

-- map' :: forall f a b. Decidable f => (f a -> f b) -> f [a] -> f [b]
-- map' f xs = chooseMaybe uncons conquer (divided onHead onTail)
--   where
--     onHead :: f b
--     onHead = _
--     onTail :: f [b]
--     onTail = _

-- minA :: forall a f. (Alternative f, Reducible a) => f a -> f a
-- minA x = red
divide0 :: Divisible f => (a -> ()) -> f a
divide0 _ = conquer
divide1 :: Contravariant f => (a -> (b)) -> f b -> f a
divide1 = contramap
divide2 :: Divisible f => (a -> (b,c)) -> f b -> f c -> f a
divide2 = divide
divide3 :: Divisible f => (a -> (b,c,d)) -> f b -> f c -> f d -> f a
divide3 split3 pb pc pd = divide (reassociate . split3) pb (divide id pc pd)
  where
    reassociate (x,y,z) = (x,(y,z))
divide4 :: Divisible f => (a -> (b,c,d,e)) -> f b -> f c -> f d -> f e -> f a
divide4 split4 pb pc pd pe =
  divide (reassociate4 . split4) pb $
  divide3 id pc pd pe
  where
    reassociate4 (x,y,z,w) = (x,(y,z,w))

divideList :: Divisible f => [(a -> b, f b)] -> f a
divideList = \case
  []           -> conquer
  ((f, px):xs) -> divide (reproject f) px (divideList xs)
  where
    reproject :: (a -> b) -> (a -> (b,a))
    reproject f = \x -> (f x, x)

decideList :: forall a b f. Decidable f => (a -> (Integer, b)) -> [f b] -> f a
decideList f (xp:xps) = choose caseConstructor xp (decideList nextF xps)
  where
    caseConstructor :: a -> Either b a
    caseConstructor x =
      let (c, w) = f x
      in case c of
        0 -> Left w
        n -> Right x
    nextF :: a -> (Integer, b)
    nextF x = let (c, w) = f x
              in (c-1, w)
    
-- divide4 :: (a -> (b,c,d,e)) -> f b -> f c -> f d -> f e -> f a
-- divide4 split bp cp dp ep = contramap split $ \(b,c,d,e) ->
--   divide _ _ _
  
-- divide3 :: (a -> (b,c,d)) -> f b -> f c -> f d -> f a
-- divide2 :: (a -> (b,c)) -> f b -> f c -> f a
-- divide1 :: (a -> (b)) -> f b -> f a
-- divide0 :: (a -> ()) -> f a



-- minimizeD :: (Refinable f, Decidable f, Reducible a) => f a -> f a
-- minimizeD pred = divide (reductions &&& id) (findD $ pred `implies` minimizeD pred) pred

minimizeD :: (Refinable f, Decidable f, Reducible a) => f a -> f a
minimizeD pred = divide (reductions &&& id) (findD $ pred `implies` minimizeD pred) pred

-- If a `divided` b means "skip b if a fails", then
-- a `implies` b means "skip b if a was successful"
class Divisible f => Refinable f where
  implies :: f a -> f a -> f a

chooseMaybe :: Decidable f => (a -> Maybe b) -> f () -> f b -> f a
chooseMaybe p nothing just = choose (maybe (Left ()) Right . p) nothing just

splitList :: [a] -> Either () (a, [a])
splitList [] = Left ()
splitList (x:xs) = Right (x, xs)
