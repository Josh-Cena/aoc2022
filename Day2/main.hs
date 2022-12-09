import Data.List
import Data.Text qualified as T
import System.Environment

main = do
  args <- getArgs
  input <- T.pack <$> (readFile $ head args)
  let games = map (\cur -> ((T.head (cur !! 0)), (T.head (cur !! 1)))) $ map (T.splitOn (T.pack " ")) (T.lines input)

  print (sum $ map score games)
  print (sum $ map (\g -> score (fst g, shape g)) games)

outcomes :: [(Char, Char, Int)]
outcomes =
  [ ('A', 'X', 3),
    ('B', 'X', 0),
    ('C', 'X', 6),
    ('A', 'Y', 6),
    ('B', 'Y', 3),
    ('C', 'Y', 0),
    ('A', 'Z', 0),
    ('B', 'Z', 6),
    ('C', 'Z', 3)
  ]

winScore :: (Char, Char) -> Int
winScore game = case find (\(opp, self, score) -> (opp, self) == game) outcomes of
  (Just (opp, self, score)) -> score
  (Nothing) -> 0

shapeScore :: Char -> Int
shapeScore 'X' = 1
shapeScore 'Y' = 2
shapeScore _ = 3

score :: (Char, Char) -> Int
score game = winScore game + shapeScore (snd game)

scoreToOutcome :: Int -> Char
scoreToOutcome 0 = 'X'
scoreToOutcome 3 = 'Y'
scoreToOutcome _ = 'Z'

shape :: (Char, Char) -> Char
shape game = case find (\(opp, self, score) -> (opp, scoreToOutcome score) == game) outcomes of
  (Just (opp, self, score)) -> self
  (Nothing) -> 'X'
