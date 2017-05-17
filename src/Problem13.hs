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
    sumDigits = foldl (+) . map digitToInt
    addRemainders sums = scanl carryForward 0 $ reverse sums

p13 :: String
p13 = concatMap show . take 10 . longSum $ numbers
  where
    numbers = lines Data.Problem13.input
