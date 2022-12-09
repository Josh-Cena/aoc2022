import Data.Char
import Data.List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Tuple
import System.Environment

main = do
  args <- getArgs
  input <- T.pack <$> (readFile $ head args)
  let matrix = addGridIndices $ map ((map digitToInt) . T.unpack) $ T.lines input
  let rowCnt = length matrix
  let colCnt = length $ head matrix
  let indices = [(r, c) | r <- [1 .. rowCnt], c <- [1 .. colCnt]]
  let grid = addBoundaries rowCnt colCnt $ Map.fromList $ concat matrix
  let recordBlocker (dx, dy) =
        let cb = addBlocker grid (dx, dy)
            indices' = if dx == 0 then indices else map swap indices
            fold = if max dx dy == 1 then foldr cb else foldl' (flip cb)
          in fold Map.empty indices'
  let blockerMaps = map recordBlocker [(0, 1), (0, -1), (1, 0), (-1, 0)]
  let visitMaps cb = foldr (cb blockerMaps (rowCnt, colCnt)) 0 indices
  print (visitMaps countVisible)
  print (visitMaps findMax)

addGridIndices :: [[a]] -> [[((Int, Int), a)]]
addGridIndices grid = grid''
  where
    grid' = addIndices $ map addIndices grid
    grid'' = map (\(r, row) -> map (\(c, val) -> ((r, c), val)) row) grid'
    addIndices = zip [1 ..]

addBoundaries :: Int -> Int -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
addBoundaries rowCnt colCnt map = foldr (flip Map.insert 10) map boundaries
  where
    boundaries =
      [(r, c) | r <- [0, rowCnt + 1], c <- [0 .. colCnt + 1]]
        ++ [(r, c) | r <- [1 .. rowCnt], c <- [0, colCnt + 1]]

isBoundary :: (Int, Int) -> (Int, Int) -> Bool
isBoundary (rowCnt, colCnt) (r, c) = r == 0 || r == rowCnt + 1 || c == 0 || c == colCnt + 1

addBlocker :: Map.Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Map.Map (Int, Int) (Int, Int) -> Map.Map (Int, Int) (Int, Int)
addBlocker grid (dx, dy) pos = Map.insert pos blockerPos
  where
    curValue = grid ! pos
    direction = tail $ iterate (\(x, y) -> (x + dx, y + dy)) pos
    blockerPos = case find (\cand -> grid ! cand >= curValue) direction of
      Just p -> p
      Nothing -> error "No blocker found"

countVisible :: [Map.Map (Int, Int) (Int, Int)] -> (Int, Int) -> (Int, Int) -> Int -> Int
countVisible blockerMaps dims pos = (+) (int isVisible)
  where
    isVisible = any (\map -> isBoundary dims (map ! pos)) blockerMaps

findMax :: [Map.Map (Int, Int) (Int, Int)] -> (Int, Int) -> (Int, Int) -> Int -> Int
findMax blockerMaps dims (x, y) = max score
  where
    score = foldr ((*) . scoreOfDir) 1 blockerMaps
    scoreOfDir map = rawScore - int (isBoundary dims (blockerX, blockerY))
      where
        (blockerX, blockerY) = map ! (x, y)
        rawScore = max (abs (blockerY - y)) (abs (blockerX - x))

(!) :: (Ord k, Show k, Show a) => Map.Map k a -> k -> a
map ! key = case Map.lookup key map of
  Just value -> value
  Nothing -> error $ "Key not found: " ++ show key ++ " in " ++ show map

int :: Bool -> Int
int True = 1
int False = 0
