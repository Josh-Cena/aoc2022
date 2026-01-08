module Day1 (solve1, solve2) where

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let tops = getTops input
  print $ tops !! 0

solve2 :: [Text] -> IO ()
solve2 input = do
  let tops = getTops input
  print $ tops !! 0 + tops !! 1 + tops !! 2

getTops :: [Text] -> [Int]
getTops input = sortOn Down tops
  where
    tops = map (sumMap readT . T.lines) $ splitT "\n\n" $ T.unlines input
