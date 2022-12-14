import Data.Char
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Utils

main = do
  input <- getInput
  let [startInput, movesInput] = splitT "\n\n" input
  let rows = map parseRow $ init $ T.lines startInput
  let columns = map (dropWhile isSpace) $ transpose rows
  let moves = map parseMove $ T.lines movesInput

  let res = foldl' (makeMove reverse) columns moves
  print $ tops res

  let res2 = foldl' (makeMove id) columns moves
  print $ tops res2

parseRow :: Text -> [Char]
parseRow c
  | T.null c = ""
  | otherwise = (T.head $ T.tail c) : parseRow (T.drop 4 c)

parseMove :: Text -> (Int, Int, Int)
parseMove m = (fst, snd, trd)
  where
    [_, fst, _, snd, _, trd] = map readT $ T.words m

makeMove :: ([Char] -> [Char]) -> [[Char]] -> (Int, Int, Int) -> [[Char]]
makeMove process columns (cnt, from, to) = before ++ col1' : middle ++ col2' : after
  where
    (before, col1 : rest) = splitAt ((min from to) - 1) columns
    (middle, col2 : after) = splitAt ((abs (from - to)) - 1) rest
    col1' = if from < to then drop cnt col1 else (process $ take cnt col2) ++ col1
    col2' = if from > to then drop cnt col2 else (process $ take cnt col1) ++ col2

tops :: [[Char]] -> String
tops = foldr (\cur acc -> (head cur) : acc) ""
