module Main where

import Planner (solve)
import Hanoi (myProblem)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . length . solve . myProblem . read . head
