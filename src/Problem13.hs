module Problem13
  ( p13
  ) where

import Data.Char (digitToInt)
import Data.List (transpose, reverse)
import qualified Data.Problem13
import Utils (dataFile)

longSum :: [String] -> [Int]
longSum numbers = addRemainders $ scanl sumDigits 0 byDigit
  where
    byDigit = reverse . transpose $ numbers
    sumDigits prev = foldl (+) prev . map digitToInt
    addRemainders sums = reverse . carryForward . reverse
    carryForward (x:y:xs)
      | x >= 10 = (x - 10) : (carryForward (y + 1) : xs)
      | otherwise = x : carryForward (y : xs)
    carryForward (x:[])
      | x >= 10 = (x - 10) : 1 : []
      | otherwise = x : []

p13 :: String
p13 = concatMap show . take 10 . longSum $ numbers
  where
    numbers = lines Data.Problem13.input
