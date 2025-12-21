module Day17(solve1, solve2) where
import Data.Text (Text)
import Data.Text qualified as T
import Data.Set (Set)
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

type Board = Set (Int, Int)
type Block = [(Int, Int)]
type State = (Int, Int, Set (Int, Int)) -- (blockIndex, windIndex, unfilledTop)

w :: Int
w = 7

-- For debugging
showBoard :: Board -> String
showBoard board = unlines [ [if S.member (x,y) board then '#' else '.' | x <- [0..w - 1]] | y <- [maxY,maxY-1..0]]
  where
    maxY = maximum (map snd (S.toList board))

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
    canMoveLeft = all (\(x, y) -> x > 0 && not (S.member (x - 1, y) board)) block
    shiftedBlock = map (\(x, y) -> (x - 1, y)) block

moveRight :: Board -> Block -> Block
moveRight board block = if canMoveRight then shiftedBlock else block
  where
    canMoveRight = all (\(x, y) -> x < w - 1 && not (S.member (x + 1, y) board)) block
    shiftedBlock = map (\(x, y) -> (x + 1, y)) block

moveDown :: Board -> Block -> Maybe Block
moveDown board block = if canMoveDown then Just shiftedBlock else Nothing
  where
    canMoveDown = all (\(x, y) -> y > 0 && not (S.member (x, y - 1) board)) block
    shiftedBlock = map (\(x, y) -> (x, y - 1)) block

dropBlock :: Board -> Block -> [(Int, Char)] -> (Block, [(Int, Char)])
dropBlock board block wind = case wind of
  [] -> error "Ran out of wind"
  dir:wind' ->
    let block' = case snd dir of
          '<' -> moveLeft board block
          '>' -> moveRight board block
          _ -> error $ "Invalid wind direction: " ++ [snd dir]
    in case moveDown board block' of
        Just block'' -> dropBlock board block'' wind'
        Nothing -> (block', wind')

solve1 :: [Text] -> IO ()
solve1 input = do
  let winds = cycle $ zip [0..] $ T.unpack $ head input
  let blocks = cycle $ zip [0..] [block1, block2, block3, block4, block5]
  let result = dropBlocks 0 S.empty winds blocks (-1) M.empty M.empty 2022
  print result

solve2 :: [Text] -> IO ()
solve2 input = do
  let winds = cycle $ zip [0..] $ T.unpack $ head input
  let blocks = cycle $ zip [0..] [block1, block2, block3, block4, block5]
  let result = dropBlocks 0 S.empty winds blocks (-1) M.empty M.empty 1_000_000_000_000
  print result

neighbors2d :: (Int, Int) -> [(Int, Int)]
neighbors2d (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

unfilledTop :: Board -> Int -> Set (Int, Int)
unfilledTop board height = bfs (Seq.singleton start) (S.singleton start)
  where
    start = (0, height)
    inBox :: (Int, Int) -> Bool
    inBox (x, y) = x >= 0 && x < w && y >= 0 && y <= height
    bfs :: Seq (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
    bfs Seq.Empty seen = seen
    bfs (p Seq.:<| q) seen =
      let next = [ n | n <- neighbors2d p, inBox n, n `S.notMember` board, n `S.notMember` seen ]
          seen' = foldr S.insert seen next
          q' = q <> Seq.fromList next
      in bfs q' seen'

dropBlocks :: Int
  -> Board
  -> [(Int, Char)]
  -> [(Int, Block)]
  -> Int
  -> Map State Int -- seenStates ! state: how many blocks have dropped at this state?
  -> Map Int Int -- heights ! dropped: what's the height after dropping this many blocks?
  -> Int
  -> Int
dropBlocks _ _ _ [] _ _ _ _ = error "Ran out of blocks"
dropBlocks blocksDropped board windSeq ((blockIndex, blockTemplate):blocksTail) top seenStates heights target
  | blocksDropped == target = top + 1
  | otherwise =
    let windSeqPos = fst (head windSeq)
        emptyTop = unfilledTop board (top + 1)
        state = (blockIndex, windSeqPos, S.map (\(x, y) -> (x, y - top)) emptyTop)

    in case M.lookup state seenStates of
          Nothing ->
            let seenStates' = M.insert state blocksDropped seenStates
                heights' = M.insert blocksDropped (top + 1) heights
                -- Drop the next block
                blockPos = top + 4
                block = map (\(x, y) -> (x + 2, y + blockPos)) blockTemplate
                (block', windSeq') = dropBlock board block windSeq
                board' = S.union board (S.fromList block')
                top' = max top $ snd $ head block'

            in dropBlocks (blocksDropped + 1) board' windSeq' blocksTail top' seenStates' heights' target

          Just cycleStart ->
            let cycleLen = blocksDropped - cycleStart
                cycleStartHeight = heights M.! cycleStart
                cycleHeight = top + 1 - cycleStartHeight
                inCycle = target - cycleStart
                numCycles = inCycle `div` cycleLen
                remainder = inCycle `mod` cycleLen
                remainderHeight = (heights M.! (cycleStart + remainder)) - cycleStartHeight
            in cycleStartHeight + numCycles * cycleHeight + remainderHeight
