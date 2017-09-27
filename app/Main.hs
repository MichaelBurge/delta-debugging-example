module Main where

import Data.IORef
import Data.List

import Chess
import Reducible
import Divisible

compareMoves :: Gamestate -> ([Move'], [Move'])
compareMoves g =
  let lefts = map printMove $ moves g
      rights = reference_moves g
  in (lefts \\ rights, rights \\ lefts)
     
testCase n = PerftTest (newGame, n)

main :: IO ()
main = do
  let g = newGame
      i = mkIterator g
  let (PerftTest (g', depth)) = minimize checkBug $ PerftTest (g, 3)
  putStrLn $ show $ g'
  putStrLn $ show $ compareMoves g'
  ticks <- readIORef counter
  putStrLn $ show ("Ticks: ", ticks)
