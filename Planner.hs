{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}
import Control.Arrow (first, second, (&&&))
import Control.Comonad (extract)
import Control.Comonad.Cofree
-- import Control.Lens
import Control.Monad.Free
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

instance Recursive Problem where
  project = build

instance Corecursive Problem where
  embed (Branch p _) = p

nextActions :: Problem -> [(Action, Problem)]
nextActions p =
  let
    actions :: [Action]
    actions = problemActions p
    paths :: [Problem]
    paths = map ((\x -> p {problemInit = x}) . (`actionCall` (problemInit p))) actions
  in
    filter ((`actionPre` (problemInit p)) . fst) . zip actions $ paths

type instance Base Problem = Tree

data Tree a = Branch Problem [(Action, a)]
  deriving (Show)

instance Functor Tree where
  fmap f (Branch p as) = Branch p $ map (second f) as

root :: Tree a -> Problem
root (Branch r _) = r

branches :: Tree a -> [(Action, a)]
branches (Branch _ b) = b
  
build ::  Problem -> Tree Problem
build p
  | isDone p = Branch p []
  | otherwise = Branch p $ zip actions branches
  where
    actions = filter (`actionPre` problemInit p) $ problemActions p
    branches = map (\x -> p {problemInit = x}) $ map actionCall actions <*> pure (problemInit p)
    -- branches = map (flip Branch [] . \x -> p {problemInit = x}) $ map actionCall actions <*> pure (problemInit p)

-- collapse :: Tree (CommandList, Problem) -> (CommandList, Problem)
-- collapse (Branch _ children) =
--   (\(a, (cs, p)) -> (a:cs, p)) . head . filter (isDone . snd . snd) $ children

-- bfs :: CommandList -> Tree Problem -> [(CommandList, Problem)]
-- bfs as tree = tbf [] [(as, tree)]
--   where
--     tbf :: [World] -> [(CommandList, Tree Problem)] -> [(CommandList, Problem)]
--     tbf seen qs = newheads ++ tbf (seen ++ map (problemInit . snd) newheads) (concatMap (annotate . second branches) qs)
--       where
--         actions = map fst qs
--         annotate :: (CommandList, [(Action, Tree Problem)]) -> [(CommandList, Tree Problem)]
--         annotate (cmds, ats) = map (first (:cmds)) ats
--         newheads = filter ((`notElem` seen) . problemInit . snd) . map (second root) $ qs

build2 :: Problem -> Tree Problem
build2 p = Branch p $ nextActions p

----

collapse :: Tree [CommandList] -> [CommandList]
collapse (Branch p as)
  | isDone p = [[]]
  | otherwise = concatMap go as
  where
    go (act, cmd) = map (act:) cmd

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

memohylo :: (Base Problem b -> b) -> (a -> Base Problem a) -> a -> b
memohylo down up x = chrono down' up' $ ([], x)
  where
    -- up' :: a -> Base Problem (Free (Base Problem) a)
    up' (seen, v) =
      let
        Branch p as = up v
      in
        if p `elem` seen
        then Branch p []
        else Branch p $ map (second (return . (\x -> (p:seen, x)))) as
    -- down' :: (Base Problem) (Cofree (Base Problem) b) -> b
    down' (Branch p as) = down (Branch p $ map (second extract) as)


-- main = print . take 20 . bfs [] . build $ myProblem 0
-- main = print . reverse . fst . head . dropWhile (not . isDone . snd) . bfs [] . build $ myProblem 4
-- main = print . build $ myProblem 0
main = print . head $ memohylo collapse build2 $ myProblem 2
