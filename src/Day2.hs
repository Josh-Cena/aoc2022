module Day2 (solve1, solve2) where

import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let games = map (\line -> let cur = T.words line in (T.head (cur !! 0), T.head (cur !! 1))) input
  print $ sumMap score games

solve2 :: [Text] -> IO ()
solve2 input = do
  let games = map (\line -> let cur = T.words line in (T.head (cur !! 0), T.head (cur !! 1))) input
  print $ sumMap (\g -> score (fst g, shape g)) games

outcomes :: [(Char, Char, Int)]
outcomes =
  [ ('A', 'X', 3),
    ('B', 'X', 0),
    ('C', 'X', 6),
    ('A', 'Y', 6),
    ('B', 'Y', 3),
    ('C', 'Y', 0),
    ('A', 'Z', 0),
    ('B', 'Z', 6),
    ('C', 'Z', 3)
  ]

winScore :: (Char, Char) -> Int
winScore game = case find (\(opp, self, _) -> (opp, self) == game) outcomes of
  (Just (_, _, score)) -> score
  Nothing -> 0

shapeScore :: Char -> Int
shapeScore 'X' = 1
shapeScore 'Y' = 2
shapeScore _ = 3

score :: (Char, Char) -> Int
score game = winScore game + shapeScore (snd game)

scoreToOutcome :: Int -> Char
scoreToOutcome 0 = 'X'
scoreToOutcome 3 = 'Y'
scoreToOutcome _ = 'Z'

shape :: (Char, Char) -> Char
shape game = case find (\(opp, _, score) -> (opp, scoreToOutcome score) == game) outcomes of
  (Just (_, self, _)) -> self
  Nothing -> 'X'
