module Day21 (solve1, solve2) where

import Control.Exception (assert)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ratio
import Data.Text (Text)
import Data.Text qualified as T
import Utils

data Monkey = Input | Operation Text Text Text
  deriving (Show)

type EvalRes = (Ratio Int, Ratio Int) -- a * humn + b

solve1 :: [Text] -> IO ()
solve1 input = do
  let (values, monkeys) = foldr parseLine (Map.empty, Map.empty) input
  let ((_, rootVal), _) = evalMonkey values monkeys (T.pack "root")
  print rootVal

solve2 :: [Text] -> IO ()
solve2 input = do
  let (values, monkeys) = foldr parseLine (Map.empty, Map.empty) input
  let rootMonkey = monkeys Map.! T.pack "root"
  let (leftName, rightName) = case rootMonkey of
        Operation left _ right -> (left, right)
        _ -> error "root should be binary"
  let values' = Map.insert (T.pack "humn") (1, 0) values
  let ((a1, b1), values'') = evalMonkey values' monkeys leftName
  let ((a2, b2), _) = evalMonkey values'' monkeys rightName
  -- Solve equation a1 * humn + b1 = a2 * humn + b2
  let humn = (b2 - b1) / (a1 - a2)
  let _ = assert (denominator humn == 1) -- should be integer
  print $ numerator humn

parseLine :: Text -> (Map Text EvalRes, Map Text Monkey) -> (Map Text EvalRes, Map Text Monkey)
parseLine line (inputs, monkeys) =
  let parts = T.words line
   in case parts of
        [name, value] ->
          let name' = T.dropEnd 1 name
              value' = readT value :: Int
              value'' = (0, fromIntegral value')
           in (Map.insert name' value'' inputs, Map.insert name' Input monkeys)
        [name, left, op, right] ->
          let name' = T.dropEnd 1 name
           in (inputs, Map.insert name' (Operation left op right) monkeys)
        _ -> error $ "Invalid line: " ++ T.unpack line

evalMonkey :: Map Text EvalRes -> Map Text Monkey -> Text -> (EvalRes, Map Text EvalRes)
evalMonkey values monkeys name = (res, Map.insert name res inputs'')
  where
    monkey = monkeys Map.! name
    (res, inputs'') = case monkey of
      Input -> (values Map.! name, values)
      Operation left op right ->
        let (leftVal, values') = evalMonkey values monkeys left
            (rightVal, values'') = evalMonkey values' monkeys right
            res' = combineRes leftVal rightVal (T.unpack op)
         in (res', values'')

combineRes :: EvalRes -> EvalRes -> String -> EvalRes
combineRes (a1, b1) (a2, b2) "+" = (a1 + a2, b1 + b2)
combineRes (a1, b1) (a2, b2) "-" = (a1 - a2, b1 - b2)
combineRes (a1, b1) (0, b2) "*" = (a1 * b2, b1 * b2)
combineRes (0, b1) (a2, b2) "*" = (a2 * b1, b2 * b1)
combineRes (a1, b1) (0, b2) "/" = (a1 / b2, b1 / b2)
combineRes _ _ op = error $ "Invalid operation or operands for " ++ op
