module Day20 (solve1, solve2) where

import Data.List (elemIndex, foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.OSTree (OSTree)
import Data.OSTree qualified as OST
import Data.Ratio (Ratio)
import Data.Text (Text)
import Utils

type Key = Ratio Integer

data TreeEntry = TreeEntry {teKey :: Key, teNum :: Int} deriving (Eq, Show)

instance Ord TreeEntry where
  compare (TreeEntry {teKey = key1}) (TreeEntry {teKey = key2}) = compare key1 key2

type CList = OSTree TreeEntry

solve :: [Int] -> Int -> Int
solve nums rounds = groveCoords zeroKey mixed
  where
    zeroId = fromJust $ elemIndex 0 nums
    ids = [0 .. length nums - 1]
    keys = map fromIntegral ids :: [Key]
    idToNum = Map.fromList $ zip ids nums
    idToKey = Map.fromList $ zip ids keys
    tree = OST.fromList $ zipWith TreeEntry keys nums
    (mixed, idToKey') = foldl' (mix idToNum) (tree, idToKey) $ concat $ replicate rounds ids
    zeroKey = idToKey' Map.! zeroId

solve1 :: [Text] -> IO ()
solve1 input = do
  let nums = map readT input :: [Int]
  print $ solve nums 1

solve2 :: [Text] -> IO ()
solve2 input = do
  let nums = map ((* 811589153) . readT) input :: [Int]
  print $ solve nums 10

mix :: Map Int Int -> (CList, Map Int Key) -> Int -> (CList, Map Int Key)
mix idToNum (tree, idToKey) id = (tree'', Map.insert id key' idToKey)
  where
    num = idToNum Map.! id
    entry = TreeEntry (idToKey Map.! id) num
    rank = fromJust $ OST.rank tree entry
    rank' = (rank + num) `mod` (OST.size tree - 1)
    tree' = OST.delete entry tree
    keyLeft = teKey $ fromJust $ OST.select tree' (rank' - 1)
    keyRight = teKey $ fromJust $ OST.select tree' rank'
    key' = if rank' == 0 then keyRight - 1 else (keyLeft + keyRight) / 2
    newEntry = TreeEntry key' num
    tree'' = OST.insert newEntry tree'

groveCoords :: Key -> CList -> Int
groveCoords zeroKey tree =
  sumMap (teNum . fromJust . OST.select tree . (`mod` n) . (+ zeroRank)) [1000, 2000, 3000]
  where
    n = OST.size tree
    zeroRank = fromJust $ OST.rank tree (TreeEntry zeroKey 0)
