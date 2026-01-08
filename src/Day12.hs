module Day12 (solve1, solve2) where

import Data.Char
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let points = concat $ addGridIndices $ map T.unpack input
  let (start, end, matrix, _) = foldr analyzePoint ((0, 0), (0, 0), Map.empty, []) points
  print $ bfs matrix end [start]

solve2 :: [Text] -> IO ()
solve2 input = do
  let points = concat $ addGridIndices $ map T.unpack input
  let (_, end, matrix, starts) = foldr analyzePoint ((0, 0), (0, 0), Map.empty, []) points
  print $ bfs matrix end starts

type Acc = ((Int, Int), (Int, Int), Map (Int, Int) Int, [(Int, Int)])

analyzePoint :: ((Int, Int), Char) -> Acc -> Acc
analyzePoint (cur, curV) (start, end, matrix, starts) = case curV of
  'E' -> (start, cur, matrixAdd 'z', starts)
  'S' -> (cur, end, matrixAdd 'a', cur : starts)
  'a' -> (start, end, matrixAdd 'a', cur : starts)
  _ -> (start, end, matrixAdd curV, starts)
  where
    matrixAdd v = Map.insert cur (ord v) matrix

bfs :: Map (Int, Int) Int -> (Int, Int) -> [(Int, Int)] -> Int
bfs matrix end starts = runQueue matrix end (Seq.fromList $ map (,0) starts) Set.empty

runQueue ::
  Map (Int, Int) Int ->
  (Int, Int) ->
  Seq ((Int, Int), Int) ->
  Set (Int, Int) ->
  Int
runQueue _ _ Seq.Empty _ = error "No path found"
runQueue matrix end ((cur, curDist) Seq.:<| rest) visited =
  if cur == end then curDist else runQueue matrix end queue' visited'
  where
    curHeight = matrix Map.! cur
    neighbors = filter canGo $ map (|+| cur) [(1, 0), (0, 1), (-1, 0), (0, -1)]
    neighborDists = map (,curDist + 1) neighbors
    visited' = Set.union visited $ Set.fromList neighbors
    queue' = rest Seq.>< Seq.fromList neighborDists

    canGo pos = case Map.lookup pos matrix of
      Nothing -> False
      Just neighborHeight -> Set.notMember pos visited && neighborHeight - curHeight <= 1

addGridIndices :: [[a]] -> [[((Int, Int), a)]]
addGridIndices grid = grid''
  where
    grid' = zip [1 ..] $ map (zip [1 ..]) grid
    grid'' = map (\(r, row) -> map (\(c, val) -> ((r, c), val)) row) grid'
