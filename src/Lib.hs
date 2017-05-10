module Lib
  ( someFunc
  , p12
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

isqrt
  :: Integral i
  => i -> i
isqrt = floor . sqrt . fromIntegral

numFactors :: Int -> Int
numFactors x =
  let top = isqrt x
      nonSquareFactors = [i | i <- [1 .. top], i < top, x `mod` i == 0]
      addSquareFactor =
        (\x ->
           if (x `mod` top == 0)
             then (x + 1)
             else x)
  in addSquareFactor . (* 2) . length $ nonSquareFactors

p12 :: Int
p12 = atLeastNFactors 500

atLeastNFactors :: Int -> Int
atLeastNFactors x = head . filter (> x) . map numFactors . scanl1 (+) $ [(250 * 250) ..]
