import Data.List
import Data.Ord
import Data.Set qualified as Set
import Data.Text qualified as T
import System.Environment

main = do
  args <- getArgs
  input <- T.pack <$> (readFile $ head args)
  let moves = map parseMove $ T.lines input
  let (chain, set) = foldl' makeMove (replicate 2 (0, 0), Set.empty) moves
  print $ Set.size set

  let (chain2, set2) = foldl' makeMove (replicate 10 (0, 0), Set.empty) moves
  print $ Set.size set2

parseMove :: T.Text -> (Char, Int)
parseMove t = (T.head fst, read $ T.unpack snd)
  where
    [fst, snd] = T.splitOn (T.pack " ") t

type State = ([(Int, Int)], Set.Set (Int, Int))

makeMove :: State -> (Char, Int) -> State
makeMove s (_, 0) = s
makeMove (chain, set) (dir, count) = makeMove (chain', Set.insert (head chain') set) (dir, count - 1)
  where
    (rest, (chainHeadX, chainHeadY)) = unsnoc chain
    chainHead' = case dir of
      'U' -> (chainHeadX, chainHeadY + 1)
      'D' -> (chainHeadX, chainHeadY - 1)
      'L' -> (chainHeadX - 1, chainHeadY)
      'R' -> (chainHeadX + 1, chainHeadY)
    chain' = foldr (\seg moved -> (moveTail seg $ head moved) : moved) [chainHead'] rest

    moveTail (tx, ty) (hx', hy')
      | max (abs dx) (abs dy) < 2 = (tx, ty)
      | otherwise = (tx + clamp (-1, 1) dx, ty + clamp (-1, 1) dy)
      where
        dx = hx' - tx
        dy = hy' - ty

unsnoc :: [a] -> ([a], a)
unsnoc [x] = ([], x)
unsnoc (x : xs) = (x : xs', last) where (xs', last) = unsnoc xs
