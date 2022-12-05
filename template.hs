import Data.Text qualified as T
import System.Environment

main = do
  args <- getArgs
  input <- T.pack <$> (readFile (args !! 0))
  print input
