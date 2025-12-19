module Day20(solve1, solve2) where
import Data.List (foldl', elemIndex)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let nums = map readT input :: [Int]
  let n = length nums
  let idxMap = M.fromList $ zip [0..] $ zip [0..] nums
  let idxMap' = foldl' (mix n) idxMap [0..n - 1]
  let newPosToNum = M.fromList $ map snd $ M.toList idxMap'
  let zeroPos = fromMaybe (error "No zero found") $ elemIndex 0 (M.elems newPosToNum)
  print $ sum $ map (\idx -> newPosToNum M.! ((zeroPos + idx) `mod` n)) [1000, 2000, 3000]

solve2 :: [Text] -> IO ()
solve2 input = do
  let nums = map ((* 811589153) . readT) input :: [Int]
  let n = length nums
  let idxMap = M.fromList $ zip [0..] $ zip [0..] nums
  let idxMap' = foldl' (\m _ -> foldl' (mix n) m [0..n - 1]) idxMap $ replicate 10 ()
  let newPosToNum = M.fromList $ map snd $ M.toList idxMap'
  let zeroPos = fromMaybe (error "No zero found") $ elemIndex 0 (M.elems newPosToNum)
  print $ sum $ map (\idx -> newPosToNum M.! ((zeroPos + idx) `mod` n)) [1000, 2000, 3000]

-- Each mixing step is O(n), not sure if this can be better
mix :: Int -> Map Int (Int, Int) -> Int -> Map Int (Int, Int)
mix n idxMap idx = idxMap'
  where
    (pos, num) = idxMap M.! idx
    -- Avoid newPos being set to n - 1, because that pushes the last element
    -- beyond the end of the list
    newPos = (pos + num) `mod` (n - 1)
    idxMap' = M.map (
      \(p, v) -> if
        | p == pos -> (newPos, v)
        | pos < newPos && pos < p && p <= newPos -> (p - 1, v)
        | pos > newPos && newPos <= p && p < pos -> (p + 1, v)
        | otherwise -> (p, v)
      ) idxMap
