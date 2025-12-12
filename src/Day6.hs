module Day6(solve1, solve2) where
import Data.Text (Text)
import Data.Text qualified as T
import Data.Set qualified as Set

solve1 :: [Text] -> IO ()
solve1 input = do
  let input' = head input
  print $ roll 4 0 input' (T.take 4 input')

solve2 :: [Text] -> IO ()
solve2 input = do
  let input' = head input
  print $ roll 14 0 input' (T.take 14 input')

roll :: Int -> Int -> Text -> Text -> Int
roll len index input prefix
  | (length . Set.fromList . T.unpack) prefix == len = index + len
  | otherwise = let rest = T.tail input in roll len (index + 1) rest (T.take len rest)
