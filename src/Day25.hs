module Day25 (solve1, solve2) where

import Data.Text (Text)
import Data.Text qualified as T

solve1 :: [Text] -> IO ()
solve1 input = do
  let snafuNumbers = map (parseSnafu . T.unpack) input
  let total = foldl (addSnafu 0) [0] snafuNumbers
  putStrLn $ showSnafu total

solve2 :: [Text] -> IO ()
solve2 _ = do
  putStrLn "No such thing, yay"

parseSnafu :: String -> [Int]
parseSnafu = reverse . map charToDigit
  where
    charToDigit '2' = 2
    charToDigit '1' = 1
    charToDigit '0' = 0
    charToDigit '-' = -1
    charToDigit '=' = -2
    charToDigit _ = error "Invalid SNAFU character"

showSnafu :: [Int] -> String
showSnafu digits = dropWhile (== '0') $ reverse $ map digitToChar digits
  where
    digitToChar 2 = '2'
    digitToChar 1 = '1'
    digitToChar 0 = '0'
    digitToChar (-1) = '-'
    digitToChar (-2) = '='
    digitToChar _ = error "Invalid SNAFU digit"

addSnafu :: Int -> [Int] -> [Int] -> [Int]
addSnafu 0 digits1 [] = digits1
addSnafu 0 [] digits2 = digits2
addSnafu carry digits1 [] = addSnafu 0 digits1 [carry]
addSnafu carry [] digits2 = addSnafu 0 [carry] digits2
addSnafu carry (d1 : ds1) (d2 : ds2) = digit : rest
  where
    sum = d1 + d2 + carry
    carry' = (sum + 2) `div` 5
    digit = (sum + 2) `mod` 5 - 2
    rest = addSnafu carry' ds1 ds2
