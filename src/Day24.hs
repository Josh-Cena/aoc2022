module Day24 (solve1, solve2) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

type State = (Int, Int, Int)

solve1 :: [Text] -> IO ()
solve1 input = do
  let board = parseBoard input
  let width = T.length (head input) - 2
  let height = length input - 2
  print $ bfs board width height (0, 1, 0) (height + 1, width)

solve2 :: [Text] -> IO ()
solve2 input = do
  let board = parseBoard input
  let width = T.length (head input) - 2
  let height = length input - 2
  let (_, _, t) =
        foldr
          (\(r, c) lastState -> (r, c, bfs board width height lastState (r, c)))
          (0, 1, 0)
          [(height + 1, width), (0, 1), (height + 1, width)]
  print t

parseBoard :: [Text] -> Map (Int, Int) Char
parseBoard input =
  Map.fromList
    [ ((r, c), ch)
      | (r, line) <- zip [0 ..] input,
        (c, ch) <- zip [0 ..] (T.unpack line)
    ]

neighbors :: State -> [State]
neighbors (r, c, t) =
  [ (r - 1, c, t + 1),
    (r + 1, c, t + 1),
    (r, c - 1, t + 1),
    (r, c + 1, t + 1),
    (r, c, t + 1)
  ]

canMoveTo :: Map (Int, Int) Char -> Int -> Int -> State -> Bool
canMoveTo board width height (r, c, t) =
  case Map.lookup (r, c) board of
    Just '#' -> False
    Nothing -> False
    _ ->
      let blockingWinds =
            [ ((r, (c - t - 1) `mod` width + 1), '>'),
              (((r - t - 1) `mod` height + 1, c), 'v'),
              ((r, (c + t - 1) `mod` width + 1), '<'),
              (((r + t - 1) `mod` height + 1, c), '^')
            ]
       in all (\(pos, wind) -> Map.lookup pos board /= Just wind) blockingWinds

bfs ::
  Map (Int, Int) Char ->
  Int ->
  Int ->
  (Int, Int, Int) ->
  (Int, Int) ->
  Int
bfs board width height start (er, ec) =
  go (Seq.singleton start) (Set.singleton start)
  where
    go :: Seq State -> Set State -> Int
    go Seq.Empty _ = error "No path found"
    go (s@(r, c, t) Seq.:<| q) seen
      | (r, c) == (er, ec) = t
      | otherwise =
          let next = [n | n <- neighbors s, canMoveTo board width height n, n `Set.notMember` seen]
              seen' = foldr Set.insert seen next
              q' = q <> Seq.fromList next
           in go q' seen'
