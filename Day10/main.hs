import Data.List
import Data.Map qualified as Map
import Data.Text qualified as T
import Utils

main = do
  input <- getInput
  let cycles = parseCycles $ T.lines input
  let history = snd $ foldl' updateRegister ((1, 1), Map.empty) cycles
  let signalSum = sumMap (\t -> t * (history ! t)) [20, 60 .. 240]
  print signalSum
  let screen = unlines $ chunksOf 40 $ map (\t -> draw t (history ! t)) [1 .. 240]
  putStrLn screen

parseCycles :: [T.Text] -> [(Int, Int)]
parseCycles [] = []
parseCycles (line : xs)
  | line == (T.pack "noop") = (1, 0) : parseCycles xs
  | otherwise = (2, readT $ T.words line !! 1) : parseCycles xs

-- state = value **during** the xth cycle
updateRegister :: ((Int, Int), Map.Map Int Int) -> (Int, Int) -> ((Int, Int), Map.Map Int Int)
updateRegister ((time, val), history) (dur, delta) = ((time', val'), history')
  where
    time' = time + dur
    val' = val + delta
    history'
      | dur == 1 = Map.insert time val history
      | dur == 2 = Map.insert (time + 1) val $ Map.insert time val history

draw :: Int -> Int -> Char
draw time spritePos = if isDrawn then '█' else ' '
  where
    pixel = time `mod` 40 - 1
    isDrawn = pixel >= spritePos - 1 && pixel <= spritePos + 1
