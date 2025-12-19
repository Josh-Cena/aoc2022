module Day16(solve1, solve2) where
import Data.Bifunctor (bimap, first)
import Data.List (find, foldl', sortBy)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let graph = M.fromList $ map parseLine input
  let meaningfulFlows = M.map fst $ M.filter (\(flow, _) -> flow > 0) graph
  let distMap = M.unions $ map (distance graph) (M.keys graph)
  let meaningfulDistMap = M.filterWithKey (\(a, b) _ -> (a == T.pack "AA" || M.member a meaningfulFlows) && M.member b meaningfulFlows && a /= b) distMap
  let meaningfulDistMap' = transformDist meaningfulDistMap
  let paths = allPaths meaningfulDistMap' meaningfulFlows 30
  print $ maximum $ map snd paths

solve2 :: [Text] -> IO ()
solve2 input = do
  let graph = M.fromList $ map parseLine input
  let meaningfulFlows = M.map fst $ M.filter (\(flow, _) -> flow > 0) graph
  let distMap = M.unions $ map (distance graph) (M.keys graph)
  let meaningfulDistMap = M.filterWithKey (\(a, b) _ -> (a == T.pack "AA" || M.member a meaningfulFlows) && M.member b meaningfulFlows && a /= b) distMap
  let meaningfulDistMap' = transformDist meaningfulDistMap
  let paths = sortBy (\(_, s1) (_, s2) -> compare s2 s1) $ map (first S.fromList) $ allPaths meaningfulDistMap' meaningfulFlows 26
  -- Not very efficient because it's O(n^2) where n ~ 60k but works
  print $ foldl' (\curMax (p1, s1) -> max curMax $ maybe 0 ((+) s1 . snd) $ find (\(p2, s2) -> s1 + s2 > curMax && S.disjoint p1 p2) paths) 0 paths

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
distance graph start = go (M.singleton (start, start) 0) (Seq.singleton start)
  where
    go dist Seq.Empty = dist
    go dist (node Seq.:<| queue) =
        let (_, neighbors) = graph M.! node
            d = dist M.! (start, node)
            unvisitedNeighbors = filter (\n -> M.notMember (start, n) dist) neighbors
            newDist = foldl (\acc neighbor -> M.insert (start, neighbor) (d + 1) acc) dist unvisitedNeighbors
            newQueue = foldl (Seq.|>) queue unvisitedNeighbors
        in go newDist newQueue

transformDist :: Map (Text, Text) Int -> Map Text (Map Text Int)
transformDist = M.foldlWithKey' insertDist M.empty
  where
    insertDist acc (a, b) d =
      let innerMap = M.findWithDefault M.empty a acc
          updatedInnerMap = M.insert b d innerMap
      in M.insert a updatedInnerMap acc

allPaths :: Map Text (Map Text Int) -> Map Text Int -> Int -> [([Text], Int)]
allPaths distMap flows time = go (T.pack "AA") time S.empty
  where
    -- state: current node, time left, opened valves
    -- current node is the last opened valve
    go :: Text -> Int -> S.Set Text -> [([Text], Int)]
    go current timeLeft opened =
      let
        neighbors = distMap M.! current
        possibleTargets = M.toList $ M.filterWithKey (\k dist -> S.notMember k opened && dist < timeLeft - 1) neighbors
        paths = concatMap (\(tgt, dist) ->
            let newOpened = S.insert tgt opened
                timeAfterMoveAndOpen = timeLeft - dist - 1
                rewardFromThisMove = (flows M.! tgt) * timeAfterMoveAndOpen
                choicesFromRest = go tgt timeAfterMoveAndOpen newOpened
            in map (bimap (tgt :) (rewardFromThisMove +)) choicesFromRest
          ) possibleTargets
      in ([], 0) : paths

isValidPair :: (([Text], Int), ([Text], Int)) -> Bool
isValidPair ((path1, _), (path2, _)) = S.disjoint (S.fromList path1) (S.fromList path2)
