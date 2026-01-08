module Utils ((|+|), (|-|), chunksOf, sumMap, readT, splitT) where

import Data.Text (Text)
import Data.Text qualified as T

(|+|) :: (Num a) => (a, a) -> (a, a) -> (a, a)
(a, b) |+| (c, d) = (a + c, b + d)

(|-|) :: (Num a) => (a, a) -> (a, a) -> (a, a)
(a, b) |-| (c, d) = (a - c, b - d)

sumMap :: (a -> Int) -> [a] -> Int
sumMap f = sum . map f

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

readT :: (Read a) => Text -> a
readT = read . T.unpack

splitT :: String -> Text -> [Text]
splitT sep = T.splitOn (T.pack sep)
