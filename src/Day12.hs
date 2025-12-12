module Day12(solve1, solve2) where
import Data.Char
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq, (><))
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
  let shortest = bfs matrix [start]
  print $ shortest ! end

solve2 :: [Text] -> IO ()
solve2 input = do
  let points = concat $ addGridIndices $ map T.unpack input
  let (_, end, matrix, starts) = foldr analyzePoint ((0, 0), (0, 0), Map.empty, []) points
  let shortest = bfs matrix starts
  print $ shortest ! end

type Acc = ((Int, Int), (Int, Int), Map (Int, Int) Int, [(Int, Int)])

analyzePoint :: ((Int, Int), Char) -> Acc -> Acc
analyzePoint (cur, curV) (start, end, matrix, starts) = case curV of
  'E' -> (start, cur, matrixAdd 'z', starts)
  'S' -> (cur, end, matrixAdd 'a', cur : starts)
  'a' -> (start, end, matrixAdd 'a', cur : starts)
  _ -> (start, end, matrixAdd curV, starts)
  where
    matrixAdd v = Map.insert cur (ord v) matrix

bfs :: Map (Int, Int) Int -> [(Int, Int)] -> Map (Int, Int) Int
bfs matrix starts =
  runQueue matrix (Seq.fromList starts, Set.fromList starts) $ Map.fromList $ map (,0) starts

runQueue :: Map (Int, Int) Int -> (Seq (Int, Int), Set (Int, Int)) -> Map (Int, Int) Int -> Map (Int, Int) Int
runQueue matrix (queue, seen) shortest
  | Seq.null queue = shortest
  | otherwise = runQueue matrix (queue', seen') shortest'
  where
    cur = queue `Seq.index` 0
    curHeight = matrix ! cur
    neighbors = map (|+| cur) [(1, 0), (0, 1), (-1, 0), (0, -1)]
    statuses = zip neighbors $ map getStatus neighbors
    visitedNeighbors = filter (>= 0) $ map snd statuses
    toVisitNeighbors = map fst $ filter ((== -1) . snd) statuses
    queue' = Seq.drop 1 queue >< Seq.fromList toVisitNeighbors
    seen' = Set.union seen $ Set.fromList toVisitNeighbors
    minDist = minimum visitedNeighbors + 1
    shortest' = Map.insertWith (\_ x -> x) cur minDist shortest

    getStatus pos = case Map.lookup pos matrix of
      -- Out of bounds; can't go there
      Nothing -> -2
      Just neighborHeight -> case Map.lookup pos shortest of
        -- Neighbor is visited; try filling current with neighbor
        Just s
          | curHeight - neighborHeight <= 1 -> s
          | otherwise -> -2
        -- Neighbor is not visited; try pushing neighbor into queue
        Nothing
          | neighborHeight - curHeight <= 1 && not (Set.member pos seen) -> -1
          | otherwise -> -2
