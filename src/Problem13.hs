module Problem13
  ( p13
  ) where

import Data.Char (digitToInt)
import Data.List (transpose, reverse)
import qualified Data.Problem13
import Utils (dataFile)

carryForward :: [Int] -> [Int]
carryForward (x:y:xs) = rem : carryForward (next : xs)
  where
    rem = x `mod` 10
    carry = x `div` 10
    next = y + carry
carryForward [x]
  | x >= 10 = carryForward [x, 0]
  | otherwise = [x]

computeSum :: [[Int]] -> [Int]
computeSum = carryBackwards . map sum
  where
    carryBackwards = reverse . carryForward . reverse

readData :: [String] -> [[Int]]
readData = map (map digitToInt) . transpose

p13 :: String
p13 = concatMap show . take 10 . computeSum . readData $ numbers
  where
    numbers = lines Data.Problem13.input
