module Problem13
  ( p13
  ) where

import Data.Char (digitToInt)
import Data.List (transpose, reverse)
import Utils (dataFile)

sumDigits :: Int -> String -> Int
sumDigits = foldl (\p d -> p + digitToInt d)

longSum :: [[Char]] -> [Int]
longSum numbers = scanl sumDigits 0 byDigit
  where
    byDigit = reverse . transpose $ numbers

p13 :: String
p13 = concatMap show . take 10 . longSum $ numbers
  where
    numbers = lines $ dataFile "problem13"
