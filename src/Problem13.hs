module Problem13
  ( p13
  ) where

import Data.Char (digitToInt)
import Data.List (transpose, reverse)
import qualified Data.Problem13
import Utils (dataFile)

carryForward :: [Int] -> [Int]
carryForward (x:y:xs)
  | x >= 10 = (x - 10) : carryForward ((y + 1) : xs)
  | otherwise = x : carryForward (y : xs)
carryForward [x]
  | x >= 10 = [x - 10, 1]
  | otherwise = [x]
carryForward [] = []

longSum :: [String] -> [Int]
longSum numbers = addRemainders $ map sumDigits byDigit
  where
    byDigit = reverse . transpose $ numbers
    sumDigits = sum . map digitToInt
    addRemainders = reverse . carryForward

p13 :: String
p13 = concatMap show . take 13 . longSum $ numbers
  where
    numbers = lines Data.Problem13.input
