import Data.Text qualified as T
import Data.Text.Read
import System.Environment
import System.IO

main = do
  args <- getArgs
  input <- T.pack <$> (readFile (args !! 0))
  print input
