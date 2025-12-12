module Day13(solve1, solve2) where
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Utils

data TreeNode = Leaf Int | Branch [TreeNode] deriving (Show, Eq)

data Token = Number Int | OpenBracket | CloseBracket deriving (Show)

instance Ord TreeNode where
  compare (Leaf a) (Leaf b) = compare a b
  compare (Branch []) (Branch []) = EQ
  compare (Branch []) (Branch _) = LT
  compare (Branch _) (Branch []) = GT
  compare (Branch (a : as)) (Branch (b : bs)) = case compare a b of
    EQ -> compare (Branch as) (Branch bs)
    other -> other
  compare la@(Leaf a) bb@(Branch b) = compare (Branch [la]) bb
  compare bb@(Branch _) la@(Leaf _) = compare bb (Branch [la])

solve1 :: [Text] -> IO ()
solve1 input = do
  let pairs = map (map parseList) $ map T.lines $ splitT "\n\n" $ T.unlines input
  let rightOrder = map (+ 1) $ findIndices (\[a, b] -> a < b) pairs
  print $ sum rightOrder

solve2 :: [Text] -> IO ()
solve2 input = do
  let pairs = map (map parseList) $ map T.lines $ splitT "\n\n" $ T.unlines input
  let d1 = Branch [Branch [Leaf 2]]
  let d2 = Branch [Branch [Leaf 6]]
  let sorted = sort $ d1 : d2 : concat pairs
  let indices = map (+ 1) $ findIndices (\a -> a == d1 || a == d2) sorted
  print $ product indices

parseList :: Text -> TreeNode
-- Technically we need to error on unconsumed input, but we can assume input is valid and program is correct
parseList text = fst $ fromJust $ makeList $ tokenize $ T.unpack text
  where
    tokenize :: String -> [Token]
    tokenize [] = []
    tokenize input@(c : rest) = case c of
      ',' -> tokenize rest
      '[' -> OpenBracket : tokenize rest
      ']' -> CloseBracket : tokenize rest
      _ -> (Number $ read token) : tokenize rest'
        where
          (token, rest') = span (\c -> (not $ elem c "[],")) input

    -- BNF:
    -- List :: OpenBracket ListElements CloseBracket
    -- ListElements :: empty | ListElement ListElements
    -- ListElement :: List | Number
    makeList :: [Token] -> Maybe (TreeNode, [Token])
    makeList (OpenBracket : next) = Just (Branch elements, rest)
      where
        (elements, CloseBracket : rest) = makeListElements next
    makeList _ = Nothing

    makeListElements tokens = case makeListElement tokens of
      Nothing -> ([], tokens)
      Just (firstElement, next) -> (firstElement : restElements, rest)
        where
          (restElements, rest) = makeListElements next

    makeListElement (Number n : rest) = Just (Leaf n, rest)
    makeListElement tokens = makeList tokens
