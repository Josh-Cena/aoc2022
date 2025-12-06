import Data.Text qualified as T
import Data.Function ((&))
import Data.List (sortOn, nub, find)
import Data.Functor ((<&>))
import Utils

main = do
  input <- getInput
  let rawLines = T.lines input
  let targetY :: Int = readT $ head rawLines
  let lines = map parseLine $ tail rawLines
  -- Each line defines a Manhattan distance circle that may intersect with y=targetY
  -- The circle is centered at (x1, y1) with r = abs(x1 - x2) + abs(y1 - y2)
  -- The intersection with y=targetY is a line segment from
  -- x1 - (r - abs(y1 - targetY)) to x1 + (r - abs(y1 - targetY)),
  -- which is valid if r >= abs(y1 - targetY)
  let segments = createSegments targetY lines
  let beaconsOnLine = lines &
        filter (\(_, _, x2, y2) -> y2 == targetY) &
        map (\(_, _, x2, _) -> x2) &
        nub
  let total = coveredCount segments - fromIntegral (length beaconsOnLine)
  print total

  let maxCoord = targetY * 2
  let res = foldr (\y acc -> case acc of
          Just _ -> acc
          Nothing ->
            let segs = createSegments y lines
                merged = mergeSegments segs
                firstPossibleSeg = find (\(a, b) -> a > 0) merged
            in case firstPossibleSeg of
              Just (a, _) | a <= maxCoord -> Just $ (a - 1) * 4000000 + y
              Nothing -> Nothing
        ) Nothing [0..maxCoord]
  case res of
    Just v -> print v
    Nothing -> print "No valid segment found"

parseLine :: T.Text -> (Int, Int, Int, Int)
parseLine line = (x1, y1, x2, y2)
  where
    [part1, part2] = T.splitOn (T.pack ": closest beacon is at x=") line
    [x1Text, y1Text] = T.splitOn (T.pack ", y=") $ T.drop (T.length (T.pack "Sensor at x=")) part1
    [x2Text, y2Text] = T.splitOn (T.pack ", y=") part2
    x1 = readT x1Text
    y1 = readT y1Text
    x2 = readT x2Text
    y2 = readT y2Text

createSegments :: Int -> [(Int, Int, Int, Int)] -> [(Int, Int)]
createSegments targetY lines = lines &
  map (\(x1, y1, x2, y2) -> (abs (x1 - x2) + abs (y1 - y2), abs (y1 - targetY), x1, y1)) &
  filter (\(r, dy, _, _) -> r >= dy) &
  map (\(r, dy, x1, _) -> (x1 - (r - dy), x1 + (r - dy)))

mergeSegments :: [(Int, Int)] -> [(Int, Int)]
mergeSegments =
  reverse . foldl step [] . sortOn fst
  where
    step [] seg = [seg]
    step ((a,b) : acc) (c,d)
      | c <= b + 1 = (a, max b d) : acc
      | otherwise  = (a,b) : (c,d) : acc

coveredCount :: [(Int, Int)] -> Int
coveredCount segments =
  mergeSegments segments &
  map (\(a, b) -> b - a + 1) &
  sum
