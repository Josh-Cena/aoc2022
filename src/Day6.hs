module Day6 (solve1, solve2) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

solve1 :: [Text] -> IO ()
solve1 input = do
  let input' = head input
  print $ roll 4 0 input'

solve2 :: [Text] -> IO ()
solve2 input = do
  let input' = head input
  print $ roll 14 0 input'

roll :: Int -> Int -> Text -> Int
roll len index input
  | (length . Set.fromList . T.unpack) prefix == len = index + len
  | otherwise = roll len (index + 1) $ T.tail input
  where
    prefix = T.take len input
