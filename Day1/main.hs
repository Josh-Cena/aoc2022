import Data.List
import Data.Ord
import Data.Text qualified as T
import Utils

main = do
  input <- getInput
  let texts = map T.lines $ T.splitOn (T.pack "\n\n") input
  let sums = map (sumMap (read . T.unpack)) texts
  let tops = sortOn Down sums

  print $ tops !! 0
  print $ tops !! 0 + tops !! 1 + tops !! 2
