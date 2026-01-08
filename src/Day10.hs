module Day10 (solve1, solve2) where

import Data.List (foldl')
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let cycles = parseCycles input
  let history = reverse $ snd $ foldl' updateRegister ((1, 1), []) cycles
  let signals = zipWith (\time val -> if time `mod` 40 == 20 then time * val else 0) [1 ..] history
  print $ sum signals

solve2 :: [Text] -> IO ()
solve2 input = do
  let cycles = parseCycles input
  let history = reverse $ snd $ foldl' updateRegister ((1, 1), []) cycles
  let screen = unlines $ chunksOf 40 $ zipWith draw [1 ..] history
  putStrLn screen

parseCycles :: [Text] -> [(Int, Int)]
parseCycles [] = []
parseCycles (line : xs)
  | line == T.pack "noop" = (1, 0) : parseCycles xs
  | otherwise = (2, readT $ T.words line !! 1) : parseCycles xs

-- state = value **during** the xth cycle
updateRegister :: ((Int, Int), [Int]) -> (Int, Int) -> ((Int, Int), [Int])
updateRegister ((time, val), history) (dur, delta) = ((time', val'), history')
  where
    time' = time + dur
    val' = val + delta
    history' = replicate dur val ++ history

draw :: Int -> Int -> Char
draw time spritePos = if isDrawn then '#' else ' '
  where
    pixel = time `mod` 40 - 1
    isDrawn = pixel >= spritePos - 1 && pixel <= spritePos + 1
