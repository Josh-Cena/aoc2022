module Day8 (solve1, solve2) where

import Data.Char
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (mapAccumL, mapAccumR, transpose, zipWith4)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Utils.Misc (count)

solve1 :: [Text] -> IO ()
solve1 input = do
  let grid = map (map digitToInt . T.unpack) input
  let lv = map leftVisible grid
  let rv = map rightVisible grid
  let grid' = transpose grid
  let tv = transpose $ map leftVisible grid'
  let bv = transpose $ map rightVisible grid'
  let visible = zipWith4 (zipWith4 (join4 (||))) lv rv tv bv
  print $ count id (concat visible)

solve2 :: [Text] -> IO ()
solve2 input = do
  let grid = map (map digitToInt . T.unpack) input
  let ld = map leftDistances grid
  let rd = map rightDistances grid
  let grid' = transpose grid
  let td = transpose $ map leftDistances grid'
  let bd = transpose $ map rightDistances grid'
  let distances = zipWith4 (zipWith4 (join4 (*))) ld rd td bd
  print $ maximum (concat distances)

scanner :: Int -> Int -> (Int, Bool)
scanner prevMax height = (max prevMax height, height > prevMax)

leftVisible :: [Int] -> [Bool]
leftVisible = snd . mapAccumL scanner (-1)

rightVisible :: [Int] -> [Bool]
rightVisible = snd . mapAccumR scanner (-1)

scanner2 :: IntMap Int -> (Int, Int) -> (IntMap Int, Int)
scanner2 lastIndex (j, height) = (lastIndex', minimum distances)
  where
    distances = [j - fromMaybe 0 (IntMap.lookup h lastIndex) | h <- [height .. 9]]
    lastIndex' = IntMap.insert height j lastIndex

leftDistances :: [Int] -> [Int]
leftDistances = snd . mapAccumL scanner2 IntMap.empty . zip [0 ..]

rightDistances :: [Int] -> [Int]
rightDistances xs = snd $ mapAccumR scanner2 IntMap.empty $ zip [(length xs - 1), (length xs - 2) .. 0] xs

join4 :: (a -> a -> a) -> a -> a -> a -> a -> a
join4 f a b c d = f (f a b) (f c d)
