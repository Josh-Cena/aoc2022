module Day9(solve1, solve2) where
import Data.List
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

parseMove :: Text -> (Char, Int)
parseMove t = (T.head fst, readT snd)
  where
    [fst, snd] = T.words t

type State = ([(Int, Int)], Set (Int, Int))

makeMove :: State -> (Char, Int) -> State
makeMove s (_, 0) = s
makeMove (chain, set) (dir, count) = makeMove (chain', Set.insert (head chain') set) (dir, count - 1)
  where
    (rest, chainHead) = unsnoc chain
    chainHead' = case dir of
      'U' -> chainHead |+| (0, 1)
      'D' -> chainHead |+| (0, -1)
      'L' -> chainHead |+| (-1, 0)
      'R' -> chainHead |+| (1, 0)
      _ -> error "Invalid direction"
    chain' = foldr (\seg moved -> moveTail seg (head moved) : moved) [chainHead'] rest

    moveTail t h'
      | max (abs dx) (abs dy) < 2 = t
      | otherwise = t |+| (clamp (-1, 1) dx, clamp (-1, 1) dy)
      where
        (dx, dy) = h' |-| t
