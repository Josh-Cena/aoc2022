module Day5(solve1, solve2) where
import Data.Char
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let (columns, moves) = parseInput $ T.unlines input
  let res = foldl' (makeMove reverse) columns moves
  putStrLn $ tops res

solve2 :: [Text] -> IO ()
solve2 input = do
  let (columns, moves) = parseInput $ T.unlines input
  let res = foldl' (makeMove id) columns moves
  putStrLn $ tops res

parseInput :: Text -> ([[Char]], [(Int, Int, Int)])
parseInput input = (columns, moves)
  where
    [startInput, movesInput] = splitT "\n\n" input
    rows = map parseRow $ init $ T.lines startInput
    columns = map (dropWhile isSpace) $ transpose rows
    moves = map parseMove $ T.lines movesInput

parseRow :: Text -> [Char]
parseRow c
  | T.null c = ""
  | otherwise = T.head (T.tail c) : parseRow (T.drop 4 c)

parseMove :: Text -> (Int, Int, Int)
parseMove m = (fst, snd, trd)
  where
    [_, fst, _, snd, _, trd] = map readT $ T.words m

makeMove :: ([Char] -> [Char]) -> [[Char]] -> (Int, Int, Int) -> [[Char]]
makeMove process columns (cnt, from, to) = before ++ col1' : middle ++ col2' : after
  where
    (before, col1 : rest) = splitAt ((min from to) - 1) columns
    (middle, col2 : after) = splitAt ((abs (from - to)) - 1) rest
    col1' = if from < to then drop cnt col1 else process (take cnt col2) ++ col1
    col2' = if from > to then drop cnt col2 else process (take cnt col1) ++ col2

tops :: [[Char]] -> String
tops = foldr (\cur acc -> head cur : acc) ""
