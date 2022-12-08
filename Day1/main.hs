import Data.List
import Data.Ord
import Data.Text qualified as T
import System.Environment

main = do
  args <- getArgs
  input <- T.pack <$> (readFile $ head args)
  let texts = map T.lines (T.splitOn (T.pack "\n\n") input)
  let sums = map sumChunk texts
  let tops = sortOn Down sums

  print (tops !! 0)
  print (tops !! 0 + tops !! 1 + tops !! 2)

sumChunk :: [T.Text] -> Int
sumChunk = sum . map (read . T.unpack)
