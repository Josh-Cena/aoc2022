module Day14 (solve1, solve2) where

import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Utils.Misc (countWhile)
import Utils

sandSource :: (Int, Int)
sandSource = (500, 0)

solve1 :: [Text] -> IO ()
solve1 input = do
  let lines = map (map ((\r -> let [x, y] = map readT r in (x, y)) . splitT ",") . splitT " -> ") input
  let grid = foldr addLine Map.empty lines
  let ymax = maximum $ map snd $ concat lines
  print $ countWhile (\(_, y) -> y < ymax) (dropSands ymax grid)

solve2 :: [Text] -> IO ()
solve2 input = do
  let lines = map (map ((\r -> let [x, y] = map readT r in (x, y)) . splitT ",") . splitT " -> ") input
  let grid = foldr addLine Map.empty lines
  let ymax = maximum $ map snd $ concat lines
  print $ 1 + countWhile (/= sandSource) (dropSands (ymax + 1) grid)

addLine :: [(Int, Int)] -> Map (Int, Int) Char -> Map (Int, Int) Char
addLine line grid = foldr (`Map.insert` '#') grid points
  where
    points = fst $ foldl' (\(ps, last) cur -> (pointsBetween last cur ++ ps, cur)) ([], head line) line

    pointsBetween (x1, y1) (x2, y2) =
      [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

dropSands :: Int -> Map (Int, Int) Char -> [(Int, Int)]
dropSands maxY grid = map snd $ iterate (\(m, _) -> dropSand maxY m) $ dropSand maxY grid

dropSand :: Int -> Map (Int, Int) Char -> (Map (Int, Int) Char, (Int, Int))
dropSand maxY grid = (Map.insert finalPos 'o' grid, finalPos)
  where
    finalPos = untilSame moveDown sandSource
    moveDown p
      | snd p == maxY = p
      | not $ Map.member down grid = down
      | not $ Map.member downLeft grid = downLeft
      | not $ Map.member downRight grid = downRight
      | otherwise = p
      where
        down = p |+| (0, 1)
        downLeft = p |+| (-1, 1)
        downRight = p |+| (1, 1)

untilSame :: (Eq a) => (a -> a) -> a -> a
untilSame f x
  | x == x' = x
  | otherwise = untilSame f x'
  where
    x' = f x
