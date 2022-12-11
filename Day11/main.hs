{-# LANGUAGE NamedFieldPuns #-}

import Data.Char
import Data.List
import Data.Ord
import Data.Map qualified as Map
import Data.Text qualified as T
import Utils

main = do
  input <- getInput
  let monkeys = Map.fromList $ map parseMonkey $ T.splitOn (T.pack "\n\n") input
  let monkeys' = foldr (const doRound) monkeys [1 .. 20]
  let times = sortOn Down $ map (\m -> inspectTimes m) $ Map.elems monkeys'
  print $ (times !! 0 * times !! 1)

data Monkey = Monkey
  { items :: [Int],
    operation :: Int -> Int,
    test :: Int,
    target1 :: Int,
    target2 :: Int,
    inspectTimes :: Int
  }

instance Show Monkey where
  show (Monkey {items, operation, test, target1, target2, inspectTimes}) = "Items " ++ show items ++ " operation 2 -> " ++ show (operation 2) ++ " test " ++ show test ++ " to " ++ show target1 ++ " or " ++ show target2 ++ " inspectTimes " ++ show inspectTimes

parseMonkey :: T.Text -> (Int, Monkey)
parseMonkey text =
  (number, Monkey {items, operation, test, target1, target2, inspectTimes = 0})
  where
    [l1, l2, l3, l4, l5, l6] = T.lines text
    number = digitToInt (T.unpack l1 !! 7)
    items = reverse $ map readT $ T.splitOn (T.pack ", ") $ T.drop (length "  Starting items: ") l2
    operation = case T.unpack $ T.drop (length "  Operation: new = old ") l3 of
      "* old" -> (^ 2)
      '*' : num -> (* (read $ tail num))
      '+' : num -> (+ (read $ tail num))
    test = readT $ T.drop (length "  Test: divisible by ") l4
    target1 = readT $ T.drop (length "    If true: throw to monkey ") l5
    target2 = readT $ T.drop (length "    If false: throw to monkey ") l6

monkeyInspect :: Monkey -> Int -> (Int, Int)
monkeyInspect (Monkey {items, operation, test, target1, target2}) item = case item' `mod` test of
  0 -> (target1, item')
  _ -> (target2, item')
  where
    item' = operation item `div` 3

monkeyDoRound :: Map.Map Int Monkey -> Int -> Map.Map Int Monkey
monkeyDoRound monkeys k = monkeys'
  where
    monkey = monkeys ! k
    targets = map (monkeyInspect monkey) $ items monkey
    monkeys' =
      Map.adjust (\m -> m {items = [], inspectTimes = inspectTimes m + length targets}) k $
        foldr throwToTarget monkeys targets
    throwToTarget (target, item) = Map.adjust (\m -> m {items = item : items m}) target

doRound :: Map.Map Int Monkey -> Map.Map Int Monkey
doRound monkeys = foldl' monkeyDoRound monkeys (Map.keys monkeys)
