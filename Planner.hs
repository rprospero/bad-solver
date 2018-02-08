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
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Debug.Trace
import System.Environment (getArgs)
  

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
  deriving (Show, Eq, Ord)
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

instance Ord Problem where
  a `compare` b = problemInit a `compare` problemInit b

nextActions :: Problem -> [(Action, Problem)]
nextActions p =
  let
    actions :: [Action]
    actions = problemActions p
    paths :: [Problem]
    paths = map ((\x -> p {problemInit = x}) . (`actionCall` (problemInit p))) actions
  in
    filter ((`actionPre` (problemInit p)) . fst) . zip actions $ paths

----

myProblem :: Int -> Problem
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
move12 :: Action
move12 = Action {
  actionName = "move top of 1 to 2",
    actionPre = move12',
    actionCall = \(World (a:as) bs cs) -> World as (a:bs) cs
  }
move13 :: Action
move13 = Action {
  actionName = "move top of 1 to 3",
    actionPre = move13',
    actionCall = \(World (a:as) bs cs) -> World as bs (a:cs)
  }
move21 :: Action
move21 = Action {
  actionName = "move top of 2 to 1",
    actionPre = move21',
    actionCall = \(World as (b:bs) cs) -> World (b:as) bs cs
  }
move23 :: Action
move23 = Action {
  actionName = "move top of 2 to 3",
    actionPre = move23',
    actionCall = \(World as (b:bs) cs) -> World as bs (b:cs)
  }
move31 :: Action
move31 = Action {
  actionName = "move top of 3 to 1",
    actionPre = move31',
    actionCall = \(World as bs (c:cs)) -> World (c:as) bs cs
  }
move32 :: Action
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

isDone :: Problem -> Bool
isDone p = problemInit p == problemGoal p

solve' :: [(CommandList, Problem)] -> State (S.Set Problem) [(CommandList, Problem)]
solve' [] = return []
solve' xs = do
  st <- get
  -- trace (show xs) $ return ()
  let go (cs, p) = [(n:cs, p') | (n, p') <- nextActions p]
  let next = filter (\x -> snd x `S.notMember` st) $ concatMap go xs
  modify (S.union (S.fromList $ map snd next))
  promise <- solve' next
  return $ xs ++ promise

solve :: Problem -> CommandList
solve p = fst . head . filter (isDone . snd) . flip evalState (S.singleton p) $ solve' [([], p)]

main :: IO ()
main = getArgs >>= print . length . solve . myProblem . read . head
