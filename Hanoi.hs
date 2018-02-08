module Hanoi where

import Control.Lens
import Planner (Action(..), Problem(..))

type World = [[Int]]

myProblem :: Int -> Int -> Problem World
myProblem discs spindles = Problem {
  problemInit = [0..discs] : replicate (spindles-1) [],
  problemGoal = reverse $ [0..discs] : replicate (spindles-1) [],
  problemActions = [
      move x y |
      x <- [0.. spindles],
      y <- [0..spindles],
      x /= y
               ]
  }

move :: Int -> Int -> Action World
move x y = Action {
  actionName = "move top of " ++ show x ++ " to " ++ show y,
  actionPre = test x y,
  actionCall = adjust x y
  }

adjust :: Int -> Int -> World -> World
adjust x y w =
  let
    Just disc = w ^? traversed.index x._head
  in
    w & traversed.index x %~ tail
      & traversed.index y %~ (disc:)

test :: Int -> Int -> World -> Bool
test x y w =
  let
    source = w ^? traversed.index x._head
    dest = w ^? traversed.index y._head
  in
    case source of
      Nothing -> False
      Just s -> case dest of
        Nothing -> True
        Just d -> d > s
      
