module Utils
  ( isqrt
  , numFactors
  , dataFile
  ) where

import Paths_project_euler

isqrt
  :: Integral i
  => i -> i
isqrt = floor . sqrt . fromIntegral

numFactors :: Int -> Int
numFactors x =
  let top = isqrt x
      nonSquareFactors = [i | i <- [1 .. top], i < top, x `mod` i == 0]
      addSquareFactor y =
        if y `mod` top == 0
          then y + 1
          else y
  in addSquareFactor . (* 2) . length $ nonSquareFactors

dataFile :: String -> IO String
dataFile file = do
  fullPath <- getDataFileName $ "data/" ++ file ++ ".txt"
  readFile fullPath
