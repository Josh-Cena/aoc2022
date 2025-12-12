module Day16(solve1, solve2) where
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
  print $ maxReward meaningfulDistMap' meaningfulFlows

solve2 :: [Text] -> IO ()
solve2 input = do
  let graph = M.fromList $ map parseLine input
  let meaningfulFlows = M.map fst $ M.filter (\(flow, _) -> flow > 0) graph
  let distMap = M.unions $ map (distance graph) (M.keys graph)
  let meaningfulDistMap = M.filterWithKey (\(a, b) _ -> (a == T.pack "AA" || M.member a meaningfulFlows) && M.member b meaningfulFlows && a /= b) distMap
  let meaningfulDistMap' = transformDist meaningfulDistMap
  print $ maxReward2 meaningfulDistMap' meaningfulFlows

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

maxReward :: Map Text (Map Text Int) -> Map Text Int -> Int
maxReward distMap flows = go (T.pack "AA") 30 S.empty
  where
    -- state: current node, time left, opened valves
    -- current node is the last opened valve
    go current timeLeft opened =
      let neighbors = distMap M.! current
          possibleMoves = M.toList $ M.filterWithKey (\k _ -> S.notMember k opened) neighbors
          rewards = map (\(next, dist) ->
              let timeAfterMoveAndOpen = timeLeft - dist - 1
              in if timeAfterMoveAndOpen <= 0 then 0
                  else
                    let newOpened = S.insert next opened
                        rewardFromThisMove = (flows M.! next) * timeAfterMoveAndOpen
                        rewardFromRest = go next timeAfterMoveAndOpen newOpened
                    in rewardFromThisMove + rewardFromRest
            ) possibleMoves
      in if null rewards then 0 else maximum rewards

maxReward2 :: Map Text (Map Text Int) -> Map Text Int -> Int
maxReward2 distMap flows = go ((T.pack "AA", 0), (T.pack "AA", 0)) 26 S.empty
  where
    -- state: (current node, t_n) (for you and elephant), time left, opened valves
    -- if t_n > 0, it means the agent is still moving to "current node"
    -- if t_n == 0, it means the agent has already opened the "current node"
    go ((cur1, 0), (cur2, t2)) timeLeft opened =
      let neighbors1 = distMap M.! cur1
          possibleMoves1 = M.toList $ M.filterWithKey (\k _ -> S.notMember k opened) neighbors1
          rewards1 = map (\(next1, dist1) ->
              go ((next1, dist1 + 1), (cur2, t2)) timeLeft (S.insert next1 opened)
            ) possibleMoves1
      in if null rewards1 then 0 else maximum rewards1
    go ((cur1, t1), (cur2, t2)) timeLeft opened
      | t1 > t2 = go ((cur2, t2), (cur1, t1)) timeLeft opened
      | t1 < t2 = if timeLeft < t1 then 0
                  else
                      go ((cur1, 0), (cur2, t2 - t1)) (timeLeft - t1) opened
                        + flows M.! cur1 * (timeLeft - t1)
      | timeLeft < t1 = 0
      | otherwise = go ((cur1, 0), (cur2, 0)) (timeLeft - t1) opened
                    + flows M.! cur1 * (timeLeft - t1)
                    + flows M.! cur2 * (timeLeft - t2)
