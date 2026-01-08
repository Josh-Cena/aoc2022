module Day4 (solve1, solve2) where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Utils.Misc
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let lines = map (map parseRange . T.split (== ',')) input
  print $ count contain lines

solve2 :: [Text] -> IO ()
solve2 input = do
  let lines = map (map parseRange . T.split (== ',')) input
  print $ count overlap lines

parseRange :: Text -> (Int, Int)
parseRange t = (s, e) where [s, e] = map readT $ T.split (== '-') t

contain, overlap :: [(Int, Int)] -> Bool
contain [(a1, b1), (a2, b2)] = (a1 <= a2 && b1 >= b2) || (a1 >= a2 && b1 <= b2)
overlap [(a1, b1), (a2, b2)] = b1 >= a2 && b2 >= a1
