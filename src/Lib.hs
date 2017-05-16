module Lib
  ( problem
  ) where

import Problem12 (p12)

problem :: String -> Maybe String
problem = flip lookup [("12", p12)]
