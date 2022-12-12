import Data.Char
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Utils

main = do
  input <- getInput
  let rucksacks = T.lines input
  let compartments = map (\line -> T.splitAt ((T.length line) `div` 2) line) rucksacks
  print $ sumMap (\(a, b) -> commonPriority (textToSet a) (textToSet b)) compartments

  let groups = map (map textToSet) $ chunksOf 3 rucksacks
  print $ sumMap (\[a, b, c] -> commonPriority a $ Set.intersection b c) groups

priority :: Char -> Int
priority c
  | v < ord 'a' = v - ord 'A' + 27
  | otherwise = v - ord 'a' + 1
  where
    v = ord c

textToSet = Set.fromList . T.unpack

commonPriority :: Set Char -> Set Char -> Int
commonPriority a b = priority $ Set.elemAt 0 $ Set.intersection a b
