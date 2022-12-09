import Data.Text qualified as T
import Data.Set qualified as Set
import System.Environment

main = do
  args <- getArgs
  input <- T.pack <$> (readFile $ head args)
  print (roll 4 0 input (T.take 4 input))
  print (roll 14 0 input (T.take 4 input))

roll :: Int -> Int -> T.Text -> T.Text -> Int
roll len index input prefix
  | (length $ Set.fromList $ T.unpack prefix) == len = index + len
  | otherwise = roll len (index + 1) (T.tail input) (T.take len $ T.tail input)
