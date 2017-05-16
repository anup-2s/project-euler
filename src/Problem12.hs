module Problem12
  ( p12
  ) where

import Utils (numFactors)

atLeastNFactors :: Int -> [Int]
atLeastNFactors x =
  let triangles = scanl1 (+) [1 ..]
  in map fst . filter (\(_, f) -> (f > x)) . zip triangles . map numFactors $ triangles

p12 :: String
p12 = show $ head . atLeastNFactors $ 500
