{-# LANGUAGE NamedFieldPuns #-}

import Data.Char
import Data.List
import Data.Map qualified as Map
import Data.Ord
import Data.Text qualified as T
import Utils

main = do
  input <- getInput
  let monkeys = Map.fromList $ map parseMonkey $ T.splitOn (T.pack "\n\n") input
  let monkeys' = passAround 20 3 monkeys
  print $ monkeyBusiness monkeys'

  let monkeys' = passAround 10000 1 monkeys
  print $ monkeyBusiness monkeys'

data Monkey = Monkey
  { items :: [Int],
    operation :: Int -> Int,
    test :: Int,
    target1 :: Int,
    target2 :: Int,
    inspectTimes :: Int
  }

instance Show Monkey where
  show (Monkey {items, operation, test, target1, target2, inspectTimes}) =
    "Items " ++ show items ++ " operation 2 -> " ++ show (operation 2) ++ " test " ++ show test ++ " to " ++ show target1 ++ " or " ++ show target2 ++ " inspectTimes " ++ show inspectTimes

parseMonkey :: T.Text -> (Int, Monkey)
parseMonkey text =
  (number, Monkey {items, operation, test, target1, target2, inspectTimes = 0})
  where
    [l1, l2, l3, l4, l5, l6] = T.lines text
    number = digitToInt (T.unpack l1 !! 7)
    items = reverse $ map readT $ T.splitOn (T.pack ", ") $ dropPrefix "  Starting items: " l2
    operation = case T.unpack $ dropPrefix "  Operation: new = old " l3 of
      "* old" -> (^ 2)
      '*' : num -> (* (read $ tail num))
      '+' : num -> (+ (read $ tail num))
    test = readAfter "  Test: divisible by " l4
    target1 = readAfter "    If true: throw to monkey " l5
    target2 = readAfter "    If false: throw to monkey " l6
    dropPrefix = T.drop . length
    readAfter prefix = readT . dropPrefix prefix

monkeyInspect :: Monkey -> (Int, Int) -> Int -> (Int, Int)
monkeyInspect (Monkey {items, operation, test, target1, target2}) (relief, divisor) item = case item' `mod` test of
  0 -> (target1, item')
  _ -> (target2, item')
  where
    item' = operation item `div` relief `mod` divisor

monkeyDoRound :: (Int, Int) -> Map.Map Int Monkey -> Int -> Map.Map Int Monkey
monkeyDoRound reducer monkeys k = updateSelf $ foldr throwToTarget monkeys targets
  where
    monkey = monkeys ! k
    targets = map (monkeyInspect monkey reducer) $ items monkey
    updateSelf = Map.adjust (\m -> m {items = [], inspectTimes = inspectTimes m + length targets}) k
    throwToTarget (target, item) = Map.adjust (\m -> m {items = item : items m}) target

doRound :: (Int, Int) -> Map.Map Int Monkey -> Map.Map Int Monkey
doRound reducer monkeys = foldl' (monkeyDoRound reducer) monkeys (Map.keys monkeys)

passAround :: Int -> Int -> Map.Map Int Monkey -> Map.Map Int Monkey
passAround rounds relief monkeys = foldr (const $ doRound (relief, divisor)) monkeys [1 .. rounds]
  where
    divisor = Map.foldr (lcm . test) 1 monkeys

monkeyBusiness :: Map.Map Int Monkey -> Int
monkeyBusiness monkeys = fst * snd
  where
    fst : snd : rest = sortOn Down $ map inspectTimes $ Map.elems monkeys
