module Main where

import Planner (solve)
import Hanoi (myProblem)
import System.Environment (getArgs)

main :: IO ()
main = do
  discs <- read . head<$> getArgs
  spindles <- read . (!! 1) <$> getArgs 
  print . length . solve $ myProblem discs spindles
