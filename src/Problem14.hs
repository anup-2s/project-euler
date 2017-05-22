module Problem14
  ( p14
  ) where

import Data.List (maximum, maximumBy)
import Data.Ord (comparing)

collatz' :: Int -> Int -> Int
collatz' i n
  | n == 1 = i
  | n `mod` 2 == 0 = collatz' (i + 1) $ n `div` 2
  | otherwise = collatz' (i + 1) $ (3 * n) + 1

collatz :: Int -> Int
collatz = collatz' 0

p14 = show . maximumBy (comparing collatz) $ [1 .. 1000000]
