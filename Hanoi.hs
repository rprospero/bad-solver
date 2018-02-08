module Hanoi where

import Planner (Action(..), Problem(..))

data World = World [Int] [Int] [Int]
  deriving (Show, Eq, Ord)

myProblem :: Int -> Problem World
myProblem n = Problem {
  problemInit = World [0..n] [] [],
  problemGoal = World [] [] [0..n],
  problemActions = [
      move12,
      move13,
      move23,
      move21,
      move31,
      move32
      ]
  }
move12 :: Action World
move12 = Action {
  actionName = "move top of 1 to 2",
    actionPre = move12',
    actionCall = \(World (a:as) bs cs) -> World as (a:bs) cs
  }
move13 :: Action World
move13 = Action {
  actionName = "move top of 1 to 3",
    actionPre = move13',
    actionCall = \(World (a:as) bs cs) -> World as bs (a:cs)
  }
move21 :: Action World
move21 = Action {
  actionName = "move top of 2 to 1",
    actionPre = move21',
    actionCall = \(World as (b:bs) cs) -> World (b:as) bs cs
  }
move23 :: Action World
move23 = Action {
  actionName = "move top of 2 to 3",
    actionPre = move23',
    actionCall = \(World as (b:bs) cs) -> World as bs (b:cs)
  }
move31 :: Action World
move31 = Action {
  actionName = "move top of 3 to 1",
    actionPre = move31',
    actionCall = \(World as bs (c:cs)) -> World (c:as) bs cs
  }
move32 :: Action World
move32 = Action {
  actionName = "move top of 3 to 2",
    actionPre = move32',
    actionCall = \(World as bs (c:cs)) -> World as (c:bs) cs
  }

type ActionPre = (World -> Bool)

move12' :: ActionPre
move12' (World (_:_) [] _) = True
move12' (World (a:_) (b:_) _) = a < b
move12' _ = False

move13' :: ActionPre
move13' (World (_:_) _ []) = True
move13' (World (a:_) _ (b:_)) = a < b
move13' _ = False

move21' :: ActionPre
move21' (World [] (_:_) _) = True
move21' (World (a:_) (b:_) _) = b < a
move21' _ = False

move23' :: ActionPre
move23' (World _ (_:_) []) = True
move23' (World _ (b:_) (c:_)) = b < c
move23' _ = False

move31' :: ActionPre
move31' (World [] _ (_:_)) = True
move31' (World (a:_) _ (c:_)) = c < a
move31' _ = False

move32' :: ActionPre
move32' (World _ [] (_:_)) = True
move32' (World _ (b:_) (c:_)) = c < b
move32' _ = False
