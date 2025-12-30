module Day23(solve1, solve2) where
import Data.Text (Text)
import Data.Text qualified as T
import Data.Set (Set)
import Data.Set qualified as S
import Data.Map qualified as M

solve1 :: [Text] -> IO ()
solve1 input = do
  let elves = parseElves input
  let directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  let (finalElves, _) = moveTimes 10 (elves, directions)
  let rs = S.map fst finalElves
  let cs = S.map snd finalElves
  let minR = S.findMin rs
  let maxR = S.findMax rs
  let minC = S.findMin cs
  let maxC = S.findMax cs
  print $ (maxR - minR + 1) * (maxC - minC + 1) - S.size finalElves

solve2 :: [Text] -> IO ()
solve2 input = do
  let elves = parseElves input
  let directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]
  print $ findStable (elves, directions)

parseElves :: [Text] -> Set (Int, Int)
parseElves input = S.fromList
  [ (r, c)
  | (r, row) <- zip [0..] input
  , (c, ch) <- zip [0..] (T.unpack row)
  , ch == '#'
  ]

canMove :: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
canMove elves (r, c) (dr, dc) = not $ any (`S.member` elves) positions
  where
    positions = case (dr, dc) of
      (-1, 0) -> [(r - 1, c - 1), (r - 1, c), (r - 1, c + 1)]
      (1, 0)  -> [(r + 1, c - 1), (r + 1, c), (r + 1, c + 1)]
      (0, -1) -> [(r - 1, c - 1), (r, c - 1), (r + 1, c - 1)]
      (0, 1)  -> [(r - 1, c + 1), (r, c + 1), (r + 1, c + 1)]
      _       -> error "Invalid direction"

proposeMove :: Set (Int, Int) -> [(Int, Int)] -> (Int, Int) -> (Int, Int)
proposeMove elves directions (r, c) = if not hasNeighbor then (r, c) else (r + dr, c + dc)
  where
    hasNeighbor = any (`S.member` elves) [(r + dr, c + dc) | dr <- [-1..1], dc <- [-1..1], (dr, dc) /= (0, 0)]
    (dr, dc) = case filter (canMove elves (r, c)) directions of
      [] -> (0, 0)
      (d:_) -> d

moveElves :: (Set (Int, Int), [(Int, Int)]) -> (Set (Int, Int), [(Int, Int)])
moveElves (elves, directions) = (elves', directions')
  where
    proposals = [(pos, proposeMove elves directions pos) | pos <- S.toList elves]
    moveCounts = foldr (\(_, pos') acc -> M.insertWith (+) pos' 1 acc) M.empty proposals
    elves' = S.fromList
      [ if M.findWithDefault 0 pos' moveCounts > 1 then pos else pos'
      | (pos, pos') <- proposals
      ]
    directions' = tail directions ++ [head directions]

moveTimes :: Int -> (Set (Int, Int), [(Int, Int)]) -> (Set (Int, Int), [(Int, Int)])
moveTimes 0 st = st
moveTimes n st = moveTimes (n - 1) (moveElves st)

findStable :: (Set (Int, Int), [(Int, Int)]) -> Int
findStable = go 1
  where
    go n s =
      let s' = moveElves s 
      in if fst s' == fst s then n else go (n + 1) s'
