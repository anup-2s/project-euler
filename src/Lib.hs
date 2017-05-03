module Lib
  ( someFunc
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

numFactors :: Int -> Int
numFactors x =
  let top = sqrt x
  in [i | i <- [1 .. floor (top)], i < top, x `mod` i == 0]

p12 :: Int -> Int
p12 x = head . filter (> 500) . map numFilters . scanl1 (+) $ [1 ..]
