import Data.List
import Data.Map qualified as Map
import Data.Text qualified as T
import System.Environment

main = do
  args <- getArgs
  input <- T.pack <$> (readFile $ head args)
  let cycles = parseCycles $ T.lines input
  let history = snd $ foldl' updateRegister ((1, 1), Map.empty) cycles
  let signalSum = sum $ map (\t -> t * (history ! t)) [20, 60 .. 240]
  print signalSum
  let screen = unlines $ chunksOf 40 $ map (\t -> draw t (history ! t)) [1 .. 240]
  putStrLn screen

parseCycles :: [T.Text] -> [(Int, Int)]
parseCycles [] = []
parseCycles (line : xs)
  | line == (T.pack "noop") = (1, 0) : parseCycles xs
  | otherwise = (2, read $ T.unpack $ (T.splitOn (T.pack " ") line) !! 1) : parseCycles xs

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

(!) :: (Ord k, Show k, Show a) => Map.Map k a -> k -> a
map ! key = case Map.lookup key map of
  Just value -> value
  Nothing -> error $ "Key not found: " ++ show key ++ " in " ++ show map
