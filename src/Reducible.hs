module Reducible where

import Chess

import Data.Int
import Data.List (find)

-- | "reduce" gives all of the single-step reductions for a type.
-- | It is recommended to put the largest reductions first.
class Reducible a where
  reductions :: a -> [a]

minimize :: Reducible a => (a -> Bool) -> a -> a
minimize f x = case find f $ reductions x of
  Nothing -> x
  Just y -> minimize f y

instance Reducible Gamestate where
  reductions g = map (applyMove g) $ moves g

instance Reducible PerftTest where
  reductions (PerftTest (g, depth)) =
    if depth == 1
    then []
    else map (\g' -> PerftTest (g', depth-1)) $ reductions g
