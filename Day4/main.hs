import Data.Text qualified as T
import System.Environment
import System.IO

main = do
  args <- getArgs
  input <- T.pack <$> (readFile $ head args)
  let lines = map (map parseRange . (T.splitOn (T.pack ","))) $ T.lines input
  print (length $ filter contain lines)
  print (length $ filter overlap lines)

parseRange :: T.Text -> (Int, Int)
parseRange t = (s, e) where [s, e] = map (read . T.unpack) (T.splitOn (T.pack "-") t)

contain :: [(Int, Int)] -> Bool
contain [(a1, b1), (a2, b2)] = (a1 <= a2 && b1 >= b2) || (a1 >= a2 && b1 <= b2)

overlap :: [(Int, Int)] -> Bool
overlap [(a1, b1), (a2, b2)] = b1 >= a2 && b2 >= a1
