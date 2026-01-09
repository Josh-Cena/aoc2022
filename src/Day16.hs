module Day16 (solve1, solve2) where

import Data.Bifunctor (second)
import Data.Bits
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let fullGraph = Map.fromList $ map parseLine input
  let graph = reduceGraph fullGraph
  let paths = allPaths graph 30
  print $ Map.foldr max 0 paths

solve2 :: [Text] -> IO ()
solve2 input = do
  let fullGraph = Map.fromList $ map parseLine input
  let graph = reduceGraph fullGraph
  let paths = allPaths graph 26
  print $
    maximum
      [ r1 + r2
        | (s1, r1) <- Map.toList paths,
          (s2, r2) <- Map.toList paths,
          disjoint s1 s2
      ]

parseLine :: Text -> (Text, (Int, [Text]))
parseLine line = (lblText, (flow, nodes))
  where
    [part1, part2] = case splitT "; tunnels lead to valves " line of
      [a, b] -> [a, b]
      _ -> splitT "; tunnel leads to valve " line
    [lblText, flowText] = splitT " has flow rate=" $ T.drop (length "Valve ") part1
    flow = readT flowText
    nodes = splitT ", " part2

newtype Node = Node Int deriving (Eq, Ord, Show)

-- "AA" is the smallest node, so it's 1 << 0
startNode :: Node
startNode = Node 1

newtype NodeSet = NodeSet Int deriving (Eq, Ord, Show)

singleton :: Node -> NodeSet
singleton (Node n) = NodeSet n

insert :: Node -> NodeSet -> NodeSet
insert (Node n) (NodeSet s) = NodeSet (s .|. n)

notMember :: Node -> NodeSet -> Bool
notMember (Node n) (NodeSet s) = (s .&. n) == 0

disjoint :: NodeSet -> NodeSet -> Bool
disjoint (NodeSet s1) (NodeSet s2) = (s1 .&. s2) == 0

distance :: Map Node (Int, [Node]) -> Node -> Map Node Int
distance graph start =
  go Map.empty (singleton start) (Seq.singleton (start, 0))
  where
    go dist _ Seq.Empty = dist
    go dist visited ((node, d) Seq.:<| queue) =
      let (flow, neighbors) = second (filter (`notMember` visited)) $ graph Map.! node
          dist' = if flow > 0 then Map.insert node d dist else dist
          visited' = foldr insert visited neighbors
          queue' = queue Seq.>< Seq.fromList (map (,d + 1) neighbors)
       in go dist' visited' queue'

-- Converts nodes to 1-hot encoding so we can use bitsets later
reduceGraph :: Map Text (Int, [Text]) -> Map Node (Int, Map Node Int)
reduceGraph fullGraph =
  encodedGraph
    & Map.filterWithKey (\k (f, _) -> k == startNode || f > 0)
    & Map.mapWithKey (\k (f, _) -> (f, distance encodedGraph k))
  where
    nodeMap = Map.fromList $ zip (Map.keys fullGraph) [Node (1 `shiftL` i) | i <- [0 ..]]
    toNode k = nodeMap Map.! k
    encodedGraph = Map.map (second (map toNode)) fullGraph & Map.mapKeys toNode

allPaths :: Map Node (Int, Map Node Int) -> Int -> Map NodeSet Int
allPaths graph time = go [(startNode, time, NodeSet 0, 0)] Map.empty
  where
    go :: [(Node, Int, NodeSet, Int)] -> Map NodeSet Int -> Map NodeSet Int
    go [] rewards = rewards
    go ((current, timeLeft, opened, prevReward) : rest) rewards =
      let (flow, neighbors) = graph Map.! current
          reward = prevReward + flow * timeLeft
          rewards' = Map.insertWith max opened reward rewards
          nextStates =
            [ (tgt, timeAfter, insert tgt opened, reward)
              | (tgt, dist) <- Map.toList neighbors,
                notMember tgt opened,
                let timeAfter = timeLeft - dist - 1,
                timeAfter > 0
            ]
       in go (nextStates ++ rest) rewards'
