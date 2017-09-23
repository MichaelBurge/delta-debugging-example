module Main where

import Chess

main :: IO ()
main = do
  let g = newGame
  putStrLn $ show $ (perft g 3, reference_perft g 3)
