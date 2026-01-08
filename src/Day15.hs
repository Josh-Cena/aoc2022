module Day15 (solve1, solve2) where

import Control.Exception (assert)
import Data.Function ((&))
import Data.List (find, nub, sortOn)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let targetY = readT $ head input
  let circles = map parseLine $ tail input
  -- Each line defines a Manhattan distance circle that may intersect with y=targetY
  -- The circle is centered at (x1, y1) with r = abs(x1 - x2) + abs(y1 - y2)
  -- The intersection with y=targetY is a line segment from
  -- x1 - (r - abs(y1 - targetY)) to x1 + (r - abs(y1 - targetY)),
  -- which is valid if r >= abs(y1 - targetY)
  let segments = createSegments targetY circles
  let beaconsOnLine =
        circles
          & filter (\(_, _, _, y2) -> y2 == targetY)
          & map (\(_, _, x2, _) -> x2)
          & nub
  let coveredCount = sumMap (\(a, b) -> b - a + 1) $ mergeSegments segments
  let total = coveredCount - length beaconsOnLine
  print total

solve2 :: [Text] -> IO ()
solve2 input = do
  let circles = map parseLine $ tail input
  let edges =
        concatMap
          ( \(x1, y1, x2, y2) ->
              let r = abs (x1 - x2) + abs (y1 - y2)
               in [ (-1, x1 + y1 + r), -- y - y1 + x - x1 = r => y = -x + x1 + y1 + r
                    (1, -x1 + y1 + r), -- y - y1 + x1 - x = r => y = x - x1 + y1 + r
                    (1, -x1 + y1 - r), -- y1 - y + x - x1 = r => y = x - x1 + y1 - r
                    (-1, x1 + y1 - r) -- y1 - y + x1 - x = r => y = -x + x1 + y1 - r
                  ]
          )
          circles ::
          [(Int, Int)]
  let pairs = [(s1, c1, c2) | (s1, c1) <- edges, (s2, c2) <- edges, s1 == s2 && c1 - c2 <= 4 && c1 - c2 > 0]
  -- Unsafe; but works on the actual input, so assume it's good enough
  -- In fact, there could be many edges that are close to each other, just that most of them
  -- have some other diamond filling in the middle.
  assert (length pairs == 2) $ pure ()
  let c1 = (\(_, c1, c1') -> (c1 + c1') `div` 2) $ fromJust $ find (\(s, _, _) -> s == 1) pairs
  let c2 = (\(_, c2, c2') -> (c2 + c2') `div` 2) $ fromJust $ find (\(s, _, _) -> s == -1) pairs
  -- x + c1 = -x + c2 => x = (c2 - c1) / 2
  let x = (c2 - c1) `div` 2
  let y = x + c1
  let tuningFreq = x * 4000000 + y
  print tuningFreq

parseLine :: Text -> (Int, Int, Int, Int)
parseLine line = (x1, y1, x2, y2)
  where
    [part1, part2] = splitT ": closest beacon is at x=" line
    [x1Text, y1Text] = splitT ", y=" $ T.drop (length "Sensor at x=") part1
    [x2Text, y2Text] = splitT ", y=" part2
    x1 = readT x1Text
    y1 = readT y1Text
    x2 = readT x2Text
    y2 = readT y2Text

createSegments :: Int -> [(Int, Int, Int, Int)] -> [(Int, Int)]
createSegments targetY circles =
  circles
    & map (\(x1, y1, x2, y2) -> (abs (x1 - x2) + abs (y1 - y2), abs (y1 - targetY), x1, y1))
    & filter (\(r, dy, _, _) -> r >= dy)
    & map (\(r, dy, x1, _) -> (x1 - (r - dy), x1 + (r - dy)))

mergeSegments :: [(Int, Int)] -> [(Int, Int)]
mergeSegments =
  reverse . foldl step [] . sortOn fst
  where
    step [] seg = [seg]
    step ((a, b) : acc) (c, d)
      | c <= b + 1 = (a, max b d) : acc
      | otherwise = (a, b) : (c, d) : acc
