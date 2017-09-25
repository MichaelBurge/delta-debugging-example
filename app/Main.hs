module Main where

import Data.IORef
import Data.List

import Chess
import Reducible
import Divisible

compareMoves :: Gamestate -> ([String], [String])
compareMoves g =
  let lefts = map printMove $ moves g
      rights = reference_moves g
  in (lefts \\ rights, rights \\ lefts)

main :: IO ()
main = do
  let g = newGame
      i = mkIterator g
  let (PerftTest (g', depth)) = minimize checkBug $ PerftTest (g, 3)
  putStrLn $ show $ g'
  ticks <- readIORef counter
  putStrLn $ show ("Ticks: ", ticks)
  putStrLn $ show $ compareMoves g'
