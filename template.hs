import Data.Text qualified as T
import System.Environment

main = do
  args <- getArgs
  input <- T.pack <$> (readFile $ head args)
  print input
