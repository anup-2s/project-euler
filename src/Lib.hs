module Lib
  ( problem
  ) where

import Problem12 (p12)
import Problem13 (p13)
import Problem14 (p14)

problem :: String -> Maybe String
problem = flip lookup [("12", p12), ("13", p13), ("14", p14)]
