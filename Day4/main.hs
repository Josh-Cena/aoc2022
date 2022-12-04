import Data.Text qualified as T
import System.Environment
import System.IO

main = do
  args <- getArgs
  input <- T.pack <$> (readFile (args !! 0))
  let lines = map (map parseRange . (T.splitOn (T.pack ","))) $ T.lines $ input
  print (length $ filter (\[r1, r2] -> contain r1 r2) $ lines)
  print (length $ filter (\[r1, r2] -> overlap r1 r2) $ lines)

parseRange :: T.Text -> (Int, Int)
parseRange t = (s, e) where [s, e] = map (read . T.unpack) (T.splitOn (T.pack "-") t)

contain :: (Int, Int) -> (Int, Int) -> Bool
contain r1 r2 = ((fst r1) <= (fst r2) && (snd r1) >= (snd r2)) || ((fst r1) >= (fst r2) && (snd r1) <= (snd r2))

overlap :: (Int, Int) -> (Int, Int) -> Bool
overlap r1 r2 = (snd r1) >= (fst r2) && (snd r2) >= (fst r1)
