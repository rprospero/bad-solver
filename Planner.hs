module Planner (solve, Action(..), Problem(..)) where

import Control.Monad.State.Strict
import qualified Data.Set as S

data Action world = Action {
  actionName :: String,
  actionPre :: world -> Bool,
  actionCall :: world -> world
  }
data Problem world = Problem {
  problemInit :: world,
  problemGoal :: world,
  problemActions :: [Action world]
}

type CommandList world = [Action world]

instance Show (Action world) where
  show = actionName


instance Eq w => Eq (Problem w) where
  a == b = problemInit a == problemInit b

instance Show w => Show (Problem w) where
  show = show . problemInit

instance Ord w => Ord (Problem w) where
  a `compare` b = problemInit a `compare` problemInit b

nextActions :: Problem w -> [(Action w, Problem w)]
nextActions p =
  let
    actions = problemActions p
    paths = map ((\x -> p {problemInit = x}) . (`actionCall` (problemInit p))) actions
  in
    filter ((`actionPre` (problemInit p)) . fst) . zip actions $ paths

----


isDone :: Eq w => Problem w -> Bool
isDone p = problemInit p == problemGoal p

solve' :: Ord w => [(CommandList w, Problem w)] -> State (S.Set (Problem w)) [(CommandList w, Problem w)]
solve' [] = return []
solve' xs = do
  st <- get
  -- trace (show xs) $ return ()
  let go (cs, p) = [(n:cs, p') | (n, p') <- nextActions p]
  let next = filter (\x -> snd x `S.notMember` st) $ concatMap go xs
  modify (S.union (S.fromList $ map snd next))
  promise <- solve' next
  return $ xs ++ promise

solve :: (Eq w, Ord w) => Problem w -> CommandList w
solve p = fst . head . filter (isDone . snd) . flip evalState (S.singleton p) $ solve' [([], p)]
