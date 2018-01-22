{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
import Control.Arrow (first, second, (&&&))
import Control.Comonad (extract)
import Control.Comonad.Cofree
-- import Control.Lens
import Control.Monad.State.Strict
import Data.Foldable (minimumBy)
import Data.Functor.Foldable
import Data.Functor.Identity
import Data.List (intercalate, nubBy, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Debug.Trace
  

-- type World = Integer

-- out :: ListF World World -> World
-- out Nil = 1
-- out (Cons x acc) = x * acc

-- collapse :: World -> ListF World World
-- collapse 0 = Nil
-- collapse x = Cons x (x-1)

-- main :: IO ()
-- main = print $ hylo out collapse 100

instance Show a => Show (State [World] a) where
  show = show . fst . flip runState []

data World = World [Int] [Int] [Int]
  deriving (Show, Eq)
data Action = Action {
  actionName :: String,
  actionPre :: World -> Bool,
  actionCall :: World -> World
}
type CommandList = [Action]

instance Show Action where
  show = actionName

data Problem = Problem {
  problemInit :: World,
  problemGoal :: World,
  problemActions :: [Action]
}

instance Eq Problem where
  a == b = problemInit a == problemInit b

instance Show Problem where
  show = show . problemInit

data Tree = Branch Problem [(Action, Tree)]
  deriving (Show)

root :: Tree -> Problem
root (Branch r _) = r

branches :: Tree -> [(Action, Tree)]
branches (Branch _ b) = b
  
build ::  Problem -> Tree
build p
  | isDone p = Branch p []
  | otherwise = Branch p $ zip actions branches
  where
    actions = filter (`actionPre` problemInit p) $ problemActions p
    branches = map (build . \x -> p {problemInit = x}) $ map actionCall actions <*> pure (problemInit p)
    -- branches = map (flip Branch [] . \x -> p {problemInit = x}) $ map actionCall actions <*> pure (problemInit p)

bfs :: CommandList -> Tree -> [(CommandList, Problem)]
bfs as tree = tbf [] [(as, tree)]
  where
    tbf :: [World] -> [(CommandList, Tree)] -> [(CommandList, Problem)]
    tbf seen qs = newheads ++ tbf (seen ++ map (problemInit . snd) newheads) (concatMap (annotate . second branches) qs)
      where
        actions = map fst qs
        annotate :: (CommandList, [(Action, Tree)]) -> [(CommandList, Tree)]
        annotate (cmds, ats) = map (first (:cmds)) ats
        newheads = filter ((`notElem` seen) . problemInit . snd) . map (second root) $ qs

myProblem n = Problem {
  problemInit = World [0..n] [] [],
  problemGoal = World [] [] [0..n],
  problemActions = [
      move12,
      move13,
      move21,
      move23,
      move31,
      move32
      ]
  }
move12 = Action {
  actionName = "move top of 1 to 2",
    actionPre = move12',
    actionCall = \(World (a:as) bs cs) -> World as (a:bs) cs
  }
move13 = Action {
  actionName = "move top of 1 to 3",
    actionPre = move13',
    actionCall = \(World (a:as) bs cs) -> World as bs (a:cs)
  }
move21 = Action {
  actionName = "move top of 2 to 1",
    actionPre = move21',
    actionCall = \(World as (b:bs) cs) -> World (b:as) bs cs
  }
move23 = Action {
  actionName = "move top of 2 to 3",
    actionPre = move23',
    actionCall = \(World as (b:bs) cs) -> World as bs (b:cs)
  }
move31 = Action {
  actionName = "move top of 3 to 1",
    actionPre = move31',
    actionCall = \(World as bs (c:cs)) -> World (c:as) bs cs
  }
move32 = Action {
  actionName = "move top of 3 to 2",
    actionPre = move32',
    actionCall = \(World as bs (c:cs)) -> World as (c:bs) cs
  }

move12' (World (a:as) [] _) = True
move12' (World (a:as) (b:bs) _) = a < b
move12' _ = False
move13' (World (a:as) _ []) = True
move13' (World (a:as) _ (b:bs)) = a < b
move13' _ = False

move21' (World [] (b:bs) _) = True
move21' (World (a:as) (b:bs) _) = b < a
move21' _ = False
move23' (World _ (b:bs) []) = True
move23' (World _ (b:bs) (c:cs)) = b < c
move23' _ = False

move31' (World [] _ (c:cs)) = True
move31' (World (a:as) _ (c:cs)) = c < a
move31' _ = False
move32' (World _ [] (c:cs)) = True
move32' (World _ (b:bs) (c:cs)) = c < b
move32' _ = False

isDone :: Problem -> Bool
isDone p = problemInit p == problemGoal p

-- main = print . take 20 . bfs [] . build $ myProblem 0
main = print . reverse . fst . head . dropWhile (not . isDone . snd) . bfs [] . build $ myProblem 4
-- main = print . build $ myProblem 0
