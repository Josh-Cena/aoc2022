module Day13 (solve1, solve2) where

import Data.List (findIndices, sort)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Utils

data TreeNode = Leaf Int | Branch [TreeNode] deriving (Show, Eq)

instance Ord TreeNode where
  compare (Leaf a) (Leaf b) = compare a b
  compare (Branch []) (Branch []) = EQ
  compare (Branch []) (Branch _) = LT
  compare (Branch _) (Branch []) = GT
  compare (Branch (a : as)) (Branch (b : bs)) = case compare a b of
    EQ -> compare (Branch as) (Branch bs)
    other -> other
  compare la@(Leaf _) bb@(Branch _) = compare (Branch [la]) bb
  compare bb@(Branch _) la@(Leaf _) = compare bb (Branch [la])

solve1 :: [Text] -> IO ()
solve1 input = do
  let pairs = map (map parseInput . T.lines) $ splitT "\n\n" $ T.unlines input
  let rightOrder = sumMap (+ 1) $ findIndices (\[a, b] -> a < b) pairs
  print rightOrder

solve2 :: [Text] -> IO ()
solve2 input = do
  let pairs = map (map parseInput . T.lines) $ splitT "\n\n" $ T.unlines input
  let d1 = Branch [Branch [Leaf 2]]
  let d2 = Branch [Branch [Leaf 6]]
  let sorted = sort $ d1 : d2 : concat pairs
  let indices = map (+ 1) $ findIndices (\a -> a == d1 || a == d2) sorted
  print $ product indices

data Token = Number Int | OpenBracket | CloseBracket deriving (Show)

tokenize :: String -> [Token]
tokenize [] = []
tokenize input@(c : rest) = case c of
  ',' -> tokenize rest
  '[' -> OpenBracket : tokenize rest
  ']' -> CloseBracket : tokenize rest
  _ -> Number (read token) : tokenize rest'
    where
      (token, rest') = span (`notElem` "[],") input

-- BNF:
-- List :: OpenBracket ListElements CloseBracket
-- ListElements :: empty | ListElement ListElements
-- ListElement :: List | Number
parseList :: [Token] -> Maybe (TreeNode, [Token])
parseList (OpenBracket : next) = Just (Branch elements, rest)
  where
    (elements, CloseBracket : rest) = parseListElements next
    parseListElements tokens = case parseListElement tokens of
      Nothing -> ([], tokens)
      Just (firstElem, next) ->
        let (restElems, restTokens) = parseListElements next
         in (firstElem : restElems, restTokens)
    parseListElement (Number n : rest) = Just (Leaf n, rest)
    parseListElement tokens = parseList tokens
parseList _ = Nothing

parseInput :: Text -> TreeNode
-- Technically we need to error on unconsumed input, but we can assume input
-- is valid and program is correct
parseInput = fst . fromJust . parseList . tokenize . T.unpack
