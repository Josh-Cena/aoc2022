module Main(main) where
import Data.Text qualified as T
import System.Environment
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23

main :: IO ()
main = do
  args <- getArgs
  let day = args !! 0
  let prob = args !! 1
  let input = if length args > 2 then args !! 2 else "real"
  let filename = "inputs/day" ++ day ++ "/" ++ input ++ ".txt"
  contentsRaw <- readFile filename
  let contents = map T.pack $ reverse $ dropWhile null $ reverse $ lines contentsRaw
  case (day, prob) of
    ("1", "1")  ->  Day1.solve1 contents
    ("1", "2")  ->  Day1.solve2 contents
    ("2", "1")  ->  Day2.solve1 contents
    ("2", "2")  ->  Day2.solve2 contents
    ("3", "1")  ->  Day3.solve1 contents
    ("3", "2")  ->  Day3.solve2 contents
    ("4", "1")  ->  Day4.solve1 contents
    ("4", "2")  ->  Day4.solve2 contents
    ("5", "1")  ->  Day5.solve1 contents
    ("5", "2")  ->  Day5.solve2 contents
    ("6", "1")  ->  Day6.solve1 contents
    ("6", "2")  ->  Day6.solve2 contents
    ("7", "1")  ->  Day7.solve1 contents
    ("7", "2")  ->  Day7.solve2 contents
    ("8", "1")  ->  Day8.solve1 contents
    ("8", "2")  ->  Day8.solve2 contents
    ("9", "1")  ->  Day9.solve1 contents
    ("9", "2")  ->  Day9.solve2 contents
    ("10", "1") -> Day10.solve1 contents
    ("10", "2") -> Day10.solve2 contents
    ("11", "1") -> Day11.solve1 contents
    ("11", "2") -> Day11.solve2 contents
    ("12", "1") -> Day12.solve1 contents
    ("12", "2") -> Day12.solve2 contents
    ("13", "1") -> Day13.solve1 contents
    ("13", "2") -> Day13.solve2 contents
    ("14", "1") -> Day14.solve1 contents
    ("14", "2") -> Day14.solve2 contents
    ("15", "1") -> Day15.solve1 contents
    ("15", "2") -> Day15.solve2 contents
    ("16", "1") -> Day16.solve1 contents
    ("16", "2") -> Day16.solve2 contents
    ("17", "1") -> Day17.solve1 contents
    ("17", "2") -> Day17.solve2 contents
    ("18", "1") -> Day18.solve1 contents
    ("18", "2") -> Day18.solve2 contents
    ("19", "1") -> Day19.solve1 contents
    ("19", "2") -> Day19.solve2 contents
    ("20", "1") -> Day20.solve1 contents
    ("20", "2") -> Day20.solve2 contents
    ("21", "1") -> Day21.solve1 contents
    ("21", "2") -> Day21.solve2 contents
    ("22", "1") -> Day22.solve1 contents
    ("22", "2") -> Day22.solve2 contents
    ("23", "1") -> Day23.solve1 contents
    ("23", "2") -> Day23.solve2 contents
    _ -> putStrLn "Day not implemented"
