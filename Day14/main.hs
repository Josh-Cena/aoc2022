import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Utils

sandSource = (500, 0)

main = do
  input <- getInput
  let lines =
        map (map (\r -> let [x, y] = map readT r in (x, y))) $
          map (map (splitT ",")) $
            map (splitT " -> ") $
              T.lines input
  let matrix = foldr addLine Map.empty lines
  let (_, (_, ymax)) = getBounds lines
  print $ countAdditions (addSand (ymax + 2) ((>= ymax) . snd)) matrix
  print $ countAdditions (addSand (ymax + 2) (== sandSource)) matrix + 1

getBounds :: [[(Int, Int)]] -> ((Int, Int), (Int, Int))
getBounds lines = foldr updateBothBounds ((1000, 0), (1000, 0)) $ concat lines
  where
    updateBothBounds (x, y) (xBounds, yBounds) =
      (updateBounds x xBounds, updateBounds y yBounds)
    updateBounds p (min, max)
      | p < min = (p, max)
      | p > max = (min, p)
      | otherwise = (min, max)

addLine :: [(Int, Int)] -> Map (Int, Int) Char -> Map (Int, Int) Char
addLine line grid = foldr (flip Map.insert '#') grid points
  where
    points = fst $ foldl' (\(ps, last) cur -> (pointsBetween last cur ++ ps, cur)) ([], line !! 0) line

    pointsBetween (x1, y1) (x2, y2) =
      [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

countAdditions :: (a -> Maybe a) -> a -> Int
countAdditions = go 0
  where
    go n f x = case f x of
      Nothing -> n
      Just x' -> go (n + 1) f x'

addSand :: Int -> ((Int, Int) -> Bool) -> Map (Int, Int) Char -> Maybe (Map (Int, Int) Char)
addSand floorY endCond grid
  | endCond finalPos = Nothing
  | otherwise = Just $ Map.insert finalPos 'o' grid
  where
    trail = iterate moveDown sandSource
    finalPos = fst $ head $ dropWhile (\(a, b) -> a /= b) $ zip trail (tail trail)
    moveDown p
      | snd p + 1 == floorY = p
      | not $ Map.member down grid = down
      | not $ Map.member downLeft grid = downLeft
      | not $ Map.member downRight grid = downRight
      | otherwise = p
      where
        down = p |+| (0, 1)
        downLeft = p |+| (-1, 1)
        downRight = p |+| (1, 1)

-- For visualization
formatGrid :: [[(Int, Int)]] -> Map (Int, Int) Char -> String
formatGrid lines matrix = unlines $ chunksOf (xmax - xmin + 1) points
  where
    matrix' = Map.insert sandSource '+' matrix
    ((xmin, xmax), (_, ymax)) = getBounds lines
    points = [fromMaybe ' ' (Map.lookup (x, y) matrix') | y <- [0 .. ymax], x <- [xmin .. xmax]]
