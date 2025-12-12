module Day10(solve1, solve2) where
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let cycles = parseCycles input
  let history = snd $ foldl' updateRegister ((1, 1), Map.empty) cycles
  let signalSum = sumMap (\t -> t * (history ! t)) [20, 60 .. 240]
  print signalSum

solve2 :: [Text] -> IO ()
solve2 input = do
  let cycles = parseCycles input
  let history = snd $ foldl' updateRegister ((1, 1), Map.empty) cycles
  let screen = unlines $ chunksOf 40 $ map (\t -> draw t (history ! t)) [1 .. 240]
  putStrLn screen

parseCycles :: [Text] -> [(Int, Int)]
parseCycles [] = []
parseCycles (line : xs)
  | line == T.pack "noop" = (1, 0) : parseCycles xs
  | otherwise = (2, readT $ T.words line !! 1) : parseCycles xs

-- state = value **during** the xth cycle
updateRegister :: ((Int, Int), Map Int Int) -> (Int, Int) -> ((Int, Int), Map Int Int)
updateRegister ((time, val), history) (dur, delta) = ((time', val'), history')
  where
    time' = time + dur
    val' = val + delta
    history'
      | dur == 1 = Map.insert time val history
      | dur == 2 = Map.insert (time + 1) val $ Map.insert time val history
      | otherwise = error "Invalid duration"

draw :: Int -> Int -> Char
draw time spritePos = if isDrawn then '█' else ' '
  where
    pixel = time `mod` 40 - 1
    isDrawn = pixel >= spritePos - 1 && pixel <= spritePos + 1
