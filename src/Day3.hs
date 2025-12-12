module Day3(solve1, solve2) where
import Data.Char
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let compartments = map (\line -> T.splitAt (T.length line `div` 2) line) input
  print $ sumMap (\(a, b) -> commonPriority (textToSet a) (textToSet b)) compartments

solve2 :: [Text] -> IO ()
solve2 input = do
  let groups = map (map textToSet) $ chunksOf 3 input
  print $ sumMap (\[a, b, c] -> commonPriority a $ Set.intersection b c) groups

priority :: Char -> Int
priority c
  | v < ord 'a' = v - ord 'A' + 27
  | otherwise = v - ord 'a' + 1
  where
    v = ord c

textToSet :: Text -> Set Char
textToSet = Set.fromList . T.unpack

commonPriority :: Set Char -> Set Char -> Int
commonPriority a b = priority $ Set.elemAt 0 $ Set.intersection a b
