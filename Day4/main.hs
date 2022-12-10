import Data.Text qualified as T
import GHC.Utils.Misc
import Utils

main = do
  input <- getInput
  let lines = map (map parseRange . (T.split (== ','))) $ T.lines input
  print $ count contain lines
  print $ count overlap lines

parseRange :: T.Text -> (Int, Int)
parseRange t = (s, e) where [s, e] = map readT $ T.split (== '-') t

contain, overlap :: [(Int, Int)] -> Bool
contain [(a1, b1), (a2, b2)] = (a1 <= a2 && b1 >= b2) || (a1 >= a2 && b1 <= b2)
overlap [(a1, b1), (a2, b2)] = b1 >= a2 && b2 >= a1
