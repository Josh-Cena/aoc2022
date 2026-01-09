module Day23 (solve1, solve2) where

import Data.List (find, mapAccumL)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

solve1 :: [Text] -> IO ()
solve1 input = do
  let elves = parseElves input
  let directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  let (finalElves, _) = moveTimes 10 (elves, directions)
  let rs = Set.map fst finalElves
  let cs = Set.map snd finalElves
  let minR = Set.findMin rs
  let maxR = Set.findMax rs
  let minC = Set.findMin cs
  let maxC = Set.findMax cs
  print $ (maxR - minR + 1) * (maxC - minC + 1) - Set.size finalElves

solve2 :: [Text] -> IO ()
solve2 input = do
  let elves = parseElves input
  let directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  print $ findStable (elves, directions)

parseElves :: [Text] -> Set (Int, Int)
parseElves input =
  Set.fromList
    [ (r, c)
      | (r, row) <- zip [0 ..] input,
        (c, ch) <- zip [0 ..] (T.unpack row),
        ch == '#'
    ]

canMove :: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
canMove elves (r, c) (dr, dc) = not $ any (`Set.member` elves) positions
  where
    positions = case (dr, dc) of
      (-1, 0) -> [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1)]
      (1, 0) -> [(r + 1, c - 1), (r + 1, c), (r + 1, c + 1)]
      (0, -1) -> [(r - 1, c - 1), (r, c - 1), (r + 1, c - 1)]
      (0, 1) -> [(r - 1, c + 1), (r, c + 1), (r + 1, c + 1)]
      _ -> error "Invalid direction"

proposeMove :: Set (Int, Int) -> [(Int, Int)] -> (Int, Int) -> (Int, Int)
proposeMove elves directions (r, c) =
  if not hasNeighbor then (r, c) else (r + dr, c + dc)
  where
    hasNeighbor =
      any
        (`Set.member` elves)
        [ (r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0)
        ]
    (dr, dc) = fromMaybe (0, 0) $ find (canMove elves (r, c)) directions

moveElves :: (Set (Int, Int), [(Int, Int)]) -> (Set (Int, Int), [(Int, Int)], Bool)
moveElves (elves, directions) = (Set.fromList elves', directions', hasMoved)
  where
    proposals = [(pos, proposeMove elves directions pos) | pos <- Set.toList elves]
    moveCounts = foldr (\(_, pos') acc -> Map.insertWith (+) pos' 1 acc) Map.empty proposals
    (hasMoved, elves') =
      mapAccumL
        ( \hasMoved (pos, pos') ->
            if Map.findWithDefault 0 pos' moveCounts > 1
              then (hasMoved, pos)
              else (hasMoved || pos /= pos', pos')
        )
        False
        proposals
    directions' = tail directions ++ [head directions]

moveTimes :: Int -> (Set (Int, Int), [(Int, Int)]) -> (Set (Int, Int), [(Int, Int)])
moveTimes 0 st = st
moveTimes n st =
  let (elves', directions', hasMoved) = moveElves st
   in if hasMoved then moveTimes (n - 1) (elves', directions') else (elves', directions')

findStable :: (Set (Int, Int), [(Int, Int)]) -> Int
findStable = go 1
  where
    go n s =
      let (elves', directions', hasMoved) = moveElves s
       in if not hasMoved then n else go (n + 1) (elves', directions')
