import Data.Char
import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import System.Environment
import System.IO

main = do
  args <- getArgs
  input <- T.pack <$> (readFile (args !! 0))
  let rucksacks = T.lines input
  let compartments = map (\line -> T.splitAt (div (T.length line) 2) line) rucksacks
  let compCommons = map (\(a, b) -> commonPriority (textToSet a) (textToSet b)) compartments
  print (sum compCommons)

  let groups = map (map textToSet) $ chunksOf 3 $ rucksacks
  let groupCommons = map (\[a, b, c] -> commonPriority a $ Set.intersection b c) groups
  print (sum groupCommons)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

priority :: Char -> Int
priority c
  | v < ord 'a' = v - ord 'A' + 27
  | otherwise = v - ord 'a' + 1
  where
    v = ord c

textToSet = Set.fromList . T.unpack

commonPriority :: Set.Set Char -> Set.Set Char -> Int
commonPriority a b = priority $ Set.elemAt 0 $ Set.intersection a b
