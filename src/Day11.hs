module Day11 (solve1, solve2) where

import Data.Char
import Data.List (foldl', sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord
import Data.Text (Text)
import Data.Text qualified as T
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let monkeys = Map.fromList $ map parseMonkey $ splitT "\n\n" $ T.unlines input
  let monkeys' = passAround 20 3 monkeys
  print $ monkeyBusiness monkeys'

solve2 :: [Text] -> IO ()
solve2 input = do
  let monkeys = Map.fromList $ map parseMonkey $ splitT "\n\n" $ T.unlines input
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

parseMonkey :: Text -> (Int, Monkey)
parseMonkey text =
  (number, Monkey {items, operation, test, target1, target2, inspectTimes = 0})
  where
    [l1, l2, l3, l4, l5, l6] = T.lines text
    number = digitToInt (T.unpack l1 !! 7)
    items = reverse $ map readT $ splitT ", " $ dropPrefix "  Starting items: " l2
    operation = case T.unpack $ dropPrefix "  Operation: new = old " l3 of
      "* old" -> (^ 2)
      '*' : num -> (* (read $ tail num))
      '+' : num -> (+ (read $ tail num))
      _ -> error "unknown operation"
    test = readAfter "  Test: divisible by " l4
    target1 = readAfter "    If true: throw to monkey " l5
    target2 = readAfter "    If false: throw to monkey " l6
    dropPrefix = T.drop . length
    readAfter prefix = readT . dropPrefix prefix

monkeyInspect :: Monkey -> (Int, Int) -> Int -> (Int, Int)
monkeyInspect (Monkey {operation, test, target1, target2}) (relief, divisor) item = (target, item')
  where
    item' = operation item `div` relief `mod` divisor
    target = if item' `mod` test == 0 then target1 else target2

monkeyDoRound :: (Int, Int) -> Map Int Monkey -> Int -> Map Int Monkey
monkeyDoRound reducer monkeys k = updateSelf $ foldr throwToTarget monkeys targets
  where
    monkey = monkeys Map.! k
    targets = map (monkeyInspect monkey reducer) $ items monkey
    updateSelf = Map.adjust (\m -> m {items = [], inspectTimes = inspectTimes m + length targets}) k
    throwToTarget (target, item) = Map.adjust (\m -> m {items = item : items m}) target

passAround :: Int -> Int -> Map Int Monkey -> Map Int Monkey
passAround rounds relief monkeys = foldr ($) monkeys $ replicate rounds $ doRound (relief, divisor)
  where
    divisor = Map.foldr ((*) . test) 1 monkeys
    doRound reducer monkeys = foldl' (monkeyDoRound reducer) monkeys (Map.keys monkeys)

monkeyBusiness :: Map Int Monkey -> Int
monkeyBusiness monkeys = fst * snd
  where
    fst : snd : _ = sortOn Down $ map inspectTimes $ Map.elems monkeys
