module Day21 (solve1, solve2) where

import Control.Exception (assert)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ratio
import Data.Text (Text)
import Data.Text qualified as T
import Utils

data Monkey = Const Int | Unknown | Operation Text Text Text deriving (Show)

type EvalRes = (Ratio Int, Ratio Int) -- a * humn + b

solve1 :: [Text] -> IO ()
solve1 input = do
  let monkeys = foldr parseLine Map.empty input
  let (_, rootVal) = evalMonkey monkeys (T.pack "root")
  print rootVal

solve2 :: [Text] -> IO ()
solve2 input = do
  let monkeys = foldr parseLine Map.empty input
  let rootMonkey = monkeys Map.! T.pack "root"
  let (leftName, rightName) = case rootMonkey of
        Operation left _ right -> (left, right)
        _ -> error "root should be binary"
  let monkeys' = Map.insert (T.pack "humn") Unknown monkeys
  let (a1, b1) = evalMonkey monkeys' leftName
  let (a2, b2) = evalMonkey monkeys' rightName
  -- Solve equation a1 * humn + b1 = a2 * humn + b2
  let humn = (b2 - b1) / (a1 - a2)
  let _ = assert (denominator humn == 1) -- should be integer
  print $ numerator humn

parseLine :: Text -> Map Text Monkey -> Map Text Monkey
parseLine line monkeys =
  case T.words line of
    [name, value] ->
      let name' = T.dropEnd 1 name
          value' = readT value :: Int
       in Map.insert name' (Const value') monkeys
    [name, left, op, right] ->
      let name' = T.dropEnd 1 name
       in Map.insert name' (Operation left op right) monkeys
    _ -> error $ "Invalid line: " ++ T.unpack line

evalMonkey :: Map Text Monkey -> Text -> EvalRes
evalMonkey monkeys name = case monkeys Map.! name of
  Const a -> (0, fromIntegral a)
  Unknown -> (1, 0)
  Operation left op right ->
    let leftVal = evalMonkey monkeys left
        rightVal = evalMonkey monkeys right
     in combineRes leftVal rightVal (T.unpack op)

combineRes :: EvalRes -> EvalRes -> String -> EvalRes
combineRes (a1, b1) (a2, b2) "+" = (a1 + a2, b1 + b2)
combineRes (a1, b1) (a2, b2) "-" = (a1 - a2, b1 - b2)
combineRes (a1, b1) (0, b2) "*" = (a1 * b2, b1 * b2)
combineRes (0, b1) (a2, b2) "*" = (a2 * b1, b2 * b1)
combineRes (a1, b1) (0, b2) "/" = (a1 / b2, b1 / b2)
combineRes _ _ op = error $ "Invalid operation " ++ op
