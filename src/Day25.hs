module Day25(solve1, solve2) where
import Data.Text (Text)
import Data.Text qualified as T

solve1 :: [Text] -> IO ()
solve1 input = do
  let snafuNumbers = map (parseSnafu . T.unpack) input
  let total = foldl addSnafu [0] snafuNumbers
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
    charToDigit _   = error "Invalid SNAFU character"

showSnafu :: [Int] -> String
showSnafu digits = dropWhile (== '0') $ reverse $ map digitToChar digits
  where
    digitToChar 2  = '2'
    digitToChar 1  = '1'
    digitToChar 0  = '0'
    digitToChar (-1) = '-'
    digitToChar (-2) = '='
    digitToChar _  = error "Invalid SNAFU digit"

addSnafu :: [Int] -> [Int] -> [Int]
addSnafu digits1 [] = digits1
addSnafu digits1 [0] = digits1
addSnafu [] digits2 = digits2
addSnafu [0] digits2 = digits2
addSnafu (d1:ds1) (d2:ds2) = digit : rest
  where
    sum = d1 + d2
    (carry, digit) = if sum > 2 then (1, sum - 5)
                    else if sum < -2 then (-1, sum + 5)
                    else (0, sum)
    rest = addSnafu [carry] $ addSnafu ds1 ds2
