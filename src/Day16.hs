module Day16 (solve1, solve2) where

import Data.Bifunctor (bimap, first)
import Data.List (find, foldl', sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let graph = Map.fromList $ map parseLine input
  let meaningfulFlows = Map.map fst $ Map.filter (\(flow, _) -> flow > 0) graph
  let distMap = Map.unions $ map (distance graph) (Map.keys graph)
  let meaningfulDistMap = Map.filterWithKey (\(a, b) _ -> (a == T.pack "AA" || Map.member a meaningfulFlows) && Map.member b meaningfulFlows && a /= b) distMap
  let meaningfulDistMap' = transformDist meaningfulDistMap
  let paths = allPaths meaningfulDistMap' meaningfulFlows 30
  print $ maximum $ map snd paths

solve2 :: [Text] -> IO ()
solve2 input = do
  let graph = Map.fromList $ map parseLine input
  let meaningfulFlows = Map.map fst $ Map.filter (\(flow, _) -> flow > 0) graph
  let distMap = Map.unions $ map (distance graph) (Map.keys graph)
  let meaningfulDistMap = Map.filterWithKey (\(a, b) _ -> (a == T.pack "AA" || Map.member a meaningfulFlows) && Map.member b meaningfulFlows && a /= b) distMap
  let meaningfulDistMap' = transformDist meaningfulDistMap
  let paths = sortBy (\(_, s1) (_, s2) -> compare s2 s1) $ map (first Set.fromList) $ allPaths meaningfulDistMap' meaningfulFlows 26
  -- Not very efficient because it's O(n^2) where n ~ 60k but works
  print $ foldl' (\curMax (p1, s1) -> max curMax $ maybe 0 ((+) s1 . snd) $ find (\(p2, s2) -> s1 + s2 > curMax && Set.disjoint p1 p2) paths) 0 paths

parseLine :: Text -> (Text, (Int, [Text]))
parseLine line = (lbl, (flow, nodes))
  where
    [part1, part2] = case T.splitOn (T.pack "; tunnels lead to valves ") line of
      [a, b] -> [a, b]
      _ -> T.splitOn (T.pack "; tunnel leads to valve ") line
    [lblText, flowText] = T.splitOn (T.pack " has flow rate=") $ T.drop (T.length (T.pack "Valve ")) part1
    lbl = lblText
    flow = readT flowText
    nodes = T.splitOn (T.pack ", ") part2

distance :: Map Text (Int, [Text]) -> Text -> Map (Text, Text) Int
distance graph start = go (Map.singleton (start, start) 0) (Seq.singleton start)
  where
    go dist Seq.Empty = dist
    go dist (node Seq.:<| queue) =
      let (_, neighbors) = graph Map.! node
          d = dist Map.! (start, node)
          unvisitedNeighbors = filter (\n -> Map.notMember (start, n) dist) neighbors
          newDist = foldl (\acc neighbor -> Map.insert (start, neighbor) (d + 1) acc) dist unvisitedNeighbors
          newQueue = foldl (Seq.|>) queue unvisitedNeighbors
       in go newDist newQueue

transformDist :: Map (Text, Text) Int -> Map Text (Map Text Int)
transformDist = Map.foldlWithKey' insertDist Map.empty
  where
    insertDist acc (a, b) d =
      let innerMap = Map.findWithDefault Map.empty a acc
          updatedInnerMap = Map.insert b d innerMap
       in Map.insert a updatedInnerMap acc

allPaths :: Map Text (Map Text Int) -> Map Text Int -> Int -> [([Text], Int)]
allPaths distMap flows time = go (T.pack "AA") time Set.empty
  where
    -- state: current node, time left, opened valves
    -- current node is the last opened valve
    go :: Text -> Int -> Set.Set Text -> [([Text], Int)]
    go current timeLeft opened =
      let neighbors = distMap Map.! current
          possibleTargets = Map.toList $ Map.filterWithKey (\k dist -> Set.notMember k opened && dist < timeLeft - 1) neighbors
          paths =
            concatMap
              ( \(tgt, dist) ->
                  let newOpened = Set.insert tgt opened
                      timeAfterMoveAndOpen = timeLeft - dist - 1
                      rewardFromThisMove = (flows Map.! tgt) * timeAfterMoveAndOpen
                      choicesFromRest = go tgt timeAfterMoveAndOpen newOpened
                   in map (bimap (tgt :) (rewardFromThisMove +)) choicesFromRest
              )
              possibleTargets
       in ([], 0) : paths
