module Main where

import Data.Maybe
import Lib

main :: IO ()
main = print . fromMaybe "Problem not solved" $ problem "15"
