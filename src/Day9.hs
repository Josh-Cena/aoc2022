module Day9 (solve1, solve2) where

import Data.List (foldl', mapAccumL)
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let moves = map parseMove input
  let (_, set) = foldl' makeMove (replicate 2 (0, 0), Set.empty) moves
  print $ Set.size set

solve2 :: [Text] -> IO ()
solve2 input = do
  let moves = map parseMove input
  let (_, set2) = foldl' makeMove (replicate 10 (0, 0), Set.empty) moves
  print $ Set.size set2

parseMove :: Text -> ((Int, Int), Int)
parseMove t = (charToDir $ T.head fst, readT snd)
  where
    [fst, snd] = T.words t

charToDir :: Char -> (Int, Int)
charToDir 'U' = (0, 1)
charToDir 'D' = (0, -1)
charToDir 'L' = (-1, 0)
charToDir 'R' = (1, 0)
charToDir _ = error "Invalid direction"

type State = ([(Int, Int)], Set (Int, Int))

makeMove :: State -> ((Int, Int), Int) -> State
makeMove s (_, 0) = s
makeMove (chainHead : rest, set) (dir, count) =
  makeMove (chainHead' : chain', Set.insert chainTail' set) (dir, count - 1)
  where
    chainHead' = chainHead |+| dir
    (chainTail', chain') =
      mapAccumL
        ( \prev cur ->
            let cur' = moveTail prev cur in (cur', cur')
        )
        chainHead'
        rest
makeMove _ _ = error "Invalid state"

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail h' t
  | max (abs dx) (abs dy) < 2 = t
  | otherwise = t |+| (clamp (-1, 1) dx, clamp (-1, 1) dy)
  where
    (dx, dy) = h' |-| t
