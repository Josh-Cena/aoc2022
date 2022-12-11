module Utils ((|+|), (|-|), (!), chunksOf, sumMap, int, unsnoc, getInput, readT) where

import Data.Map qualified as Map
import Data.Text qualified as T
import System.Environment

(|+|) :: (Num a) => (a, a) -> (a, a) -> (a, a)
(a, b) |+| (c, d) = (a + c, b + d)

(|-|) :: (Num a) => (a, a) -> (a, a) -> (a, a)
(a, b) |-| (c, d) = (a - c, b - d)

(!) :: (Ord k, Show k, Show a) => Map.Map k a -> k -> a
map ! key = case Map.lookup key map of
  Just value -> value
  Nothing -> error $ "Key not found: " ++ show key ++ " in " ++ show map

sumMap :: (a -> Int) -> [a] -> Int
sumMap f = sum . map f

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

int :: Bool -> Int
int True = 1
int False = 0

unsnoc :: [a] -> ([a], a)
unsnoc [x] = ([], x)
unsnoc (x : xs) = (x : xs', last) where (xs', last) = unsnoc xs

getInput :: IO T.Text
getInput = do
  args <- getArgs
  T.pack <$> (readFile $ head args)

readT :: Read a => T.Text -> a
readT = read . T.unpack