module Day17 (solve1, solve2) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Utils

type Board = Set (Int, Int)

type Block = [(Int, Int)]

type State = (Int, Int, Set (Int, Int)) -- (blockIndex, windIndex, unfilledTop)

w :: Int
w = 7

-- For debugging
showBoard :: Board -> String
showBoard board = unlines [[if Set.member (x, y) board then '#' else '.' | x <- [0 .. w - 1]] | y <- [maxY, maxY - 1 .. 0]]
  where
    maxY = maximum (map snd (Set.toList board))

-- ####
block1 :: Block
block1 = [(0, 0), (1, 0), (2, 0), (3, 0)]

-- .#.
-- ###
-- .#.
block2 :: Block
block2 = [(1, 2), (0, 1), (1, 1), (2, 1), (1, 0)]

-- ..#
-- ..#
-- ###
block3 :: Block
block3 = [(2, 2), (2, 1), (0, 0), (1, 0), (2, 0)]

-- #
-- #
-- #
-- #
block4 :: Block
block4 = [(0, 3), (0, 2), (0, 1), (0, 0)]

-- ##
-- ##
block5 :: Block
block5 = [(0, 1), (1, 1), (0, 0), (1, 0)]

moveLeft :: Board -> Block -> Block
moveLeft board block = if canMoveLeft then shiftedBlock else block
  where
    canMoveLeft = all (\(x, y) -> x > 0 && not (Set.member (x - 1, y) board)) block
    shiftedBlock = map (|-| (1, 0)) block

moveRight :: Board -> Block -> Block
moveRight board block = if canMoveRight then shiftedBlock else block
  where
    canMoveRight = all (\(x, y) -> x < w - 1 && not (Set.member (x + 1, y) board)) block
    shiftedBlock = map (|+| (1, 0)) block

moveDown :: Board -> Block -> Maybe Block
moveDown board block = if canMoveDown then Just shiftedBlock else Nothing
  where
    canMoveDown = all (\(x, y) -> y > 0 && not (Set.member (x, y - 1) board)) block
    shiftedBlock = map (|-| (0, 1)) block

dropBlock :: Board -> Block -> [(Int, Char)] -> (Block, [(Int, Char)])
dropBlock board block wind = case wind of
  [] -> error "Ran out of wind"
  dir : wind' ->
    let block' = case snd dir of
          '<' -> moveLeft board block
          '>' -> moveRight board block
          _ -> error $ "Invalid wind direction: " ++ [snd dir]
     in case moveDown board block' of
          Just block'' -> dropBlock board block'' wind'
          Nothing -> (block', wind')

solve1 :: [Text] -> IO ()
solve1 input = do
  let winds = cycle $ zip [0 ..] $ T.unpack $ head input
  let blocks = cycle $ zip [0 ..] [block1, block2, block3, block4, block5]
  let result = dropBlocks 0 Set.empty winds blocks 0 Map.empty Map.empty 2022
  print result

solve2 :: [Text] -> IO ()
solve2 input = do
  let winds = cycle $ zip [0 ..] $ T.unpack $ head input
  let blocks = cycle $ zip [0 ..] [block1, block2, block3, block4, block5]
  let result = dropBlocks 0 Set.empty winds blocks 0 Map.empty Map.empty 1_000_000_000_000
  print result

neighbors2d :: (Int, Int) -> [(Int, Int)]
neighbors2d (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

unfilledTop :: Board -> Int -> Set (Int, Int)
unfilledTop board height = bfs (Seq.singleton start) (Set.singleton start)
  where
    start = (0, height)
    inBox :: (Int, Int) -> Bool
    inBox (x, y) = x >= 0 && x < w && y >= 0 && y <= height
    bfs :: Seq (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
    bfs Seq.Empty seen = seen
    bfs (p Seq.:<| q) seen =
      let next = [n | n <- neighbors2d p, inBox n, Set.notMember n board, Set.notMember n seen]
          seen' = foldr Set.insert seen next
          q' = q Seq.>< Seq.fromList next
       in bfs q' seen'

dropBlocks ::
  Int ->
  Board ->
  [(Int, Char)] ->
  [(Int, Block)] ->
  Int ->
  Map State Int -> -- seenStates ! state: how many blocks have dropped at this state?
  Map Int Int -> -- heights ! dropped: what's the height after dropping this many blocks?
  Int ->
  Int
dropBlocks _ _ _ [] _ _ _ _ = error "Ran out of blocks"
dropBlocks blocksDropped board windSeq ((blockIndex, blockTemplate) : blocksTail) height seenStates heights target
  | blocksDropped == target = height
  | otherwise =
      let windSeqPos = fst (head windSeq)
          emptyTop = unfilledTop board height
          state = (blockIndex, windSeqPos, Set.map (\(x, y) -> (x, y - height)) emptyTop)
       in case Map.lookup state seenStates of
            Nothing ->
              let seenStates' = Map.insert state blocksDropped seenStates
                  heights' = Map.insert blocksDropped height heights
                  -- Drop the next block
                  block = map (|+| (2, height + 3)) blockTemplate
                  (block', windSeq') = dropBlock board block windSeq
                  board' = Set.union board (Set.fromList block')
                  height' = max height $ snd (head block') + 1
               in dropBlocks (blocksDropped + 1) board' windSeq' blocksTail height' seenStates' heights' target
            Just cycleStart ->
              let cycleLen = blocksDropped - cycleStart
                  cycleStartHeight = heights Map.! cycleStart
                  cycleHeight = height - cycleStartHeight
                  inCycle = target - cycleStart
                  numCycles = inCycle `div` cycleLen
                  remainder = inCycle `mod` cycleLen
                  remainderHeight = (heights Map.! (cycleStart + remainder)) - cycleStartHeight
               in cycleStartHeight + numCycles * cycleHeight + remainderHeight
