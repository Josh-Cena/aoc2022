module Day22(solve1, solve2) where
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Utils

-- (row, column, direction)
-- Directions: 0=right, 1=down, 2=left, 3=up
type State = (Int, Int, Int)

straightWraparound :: Map State State
straightWraparound = M.fromList (
  [ ((r, 50, 2), (r, 150, 2))  | r <- [1..50] ] ++
  [ ((r, 151, 0), (r, 51, 0))  | r <- [1..50] ] ++
  [ ((r, 50, 2), (r, 100, 2))  | r <- [51..100] ] ++
  [ ((r, 101, 0), (r, 51, 0))  | r <- [51..100] ] ++
  [ ((r, 0, 2), (r, 100, 2))   | r <- [101..150] ] ++
  [ ((r, 101, 0), (r, 1, 0))   | r <- [101..150] ] ++
  [ ((r, 0, 2), (r, 50, 2))    | r <- [151..200] ] ++
  [ ((r, 51, 0), (r, 1, 0))    | r <- [151..200] ] ++
  [ ((100, c, 3), (200, c, 3)) | c <- [1..50] ] ++
  [ ((201, c, 1), (101, c, 1)) | c <- [1..50] ] ++
  [ ((0, c, 3), (150, c, 3))   | c <- [51..100] ] ++
  [ ((151, c, 1), (1, c, 1))   | c <- [51..100] ] ++
  [ ((0, c, 3), (50, c, 3))    | c <- [101..150] ] ++
  [ ((51, c, 1), (1, c, 1))    | c <- [101..150] ]
  )

--   BBAA
--   BBAA
--   CC
--   CC
-- EEDD
-- EEDD
-- FF
-- FF
cubeWraparound :: Map State State
cubeWraparound = M.fromList (
  [ ((r, 50, 2), (151 - r, 1, 0))    | r <- [1..50] ] ++ -- (B, left) -- (E, left')
  [ ((r, 151, 0), (151 - r, 100, 2)) | r <- [1..50] ] ++ -- (A, right) -- (D, right')
  [ ((r, 50, 2), (101, r - 50, 1))   | r <- [51..100] ] ++ -- (C, left) -- (E, up)
  [ ((r, 101, 0), (50, r + 50, 3))   | r <- [51..100] ] ++ -- (C, right) -- (A, down)
  [ ((r, 0, 2), (151 - r, 51, 0))    | r <- [101..150] ] ++ -- (E, left) -- (B, left')
  [ ((r, 101, 0), (151 - r, 150, 2)) | r <- [101..150] ] ++ -- (D, right) -- (A, right')
  [ ((r, 0, 2), (1, r - 100, 1))     | r <- [151..200] ] ++ -- (F, left) -- (B, up)
  [ ((r, 51, 0), (150, r - 100, 3))  | r <- [151..200] ] ++ -- (F, right) -- (D, down)
  [ ((100, c, 3), (c + 50, 51, 0))   | c <- [1..50] ] ++ -- (E, up) -- (C, left)
  [ ((201, c, 1), (1, c + 100, 1))   | c <- [1..50] ] ++ -- (F, down) -- (A, up)
  [ ((0, c, 3), (c + 100, 1, 0))     | c <- [51..100] ] ++ -- (B, up) -- (F, left)
  [ ((151, c, 1), (c + 100, 50, 2))  | c <- [51..100] ] ++ -- (D, down) -- (F, right)
  [ ((0, c, 3), (200, c - 100, 3))   | c <- [101..150] ] ++ -- (A, up) -- (F, down)
  [ ((51, c, 1), (c - 50, 100, 2))   | c <- [101..150] ] -- (A, down) -- (C, right)
  )

solve :: Map State State -> [Text] -> Int
solve wraparound input = finalScore
  where
    chunks = splitT "\n\n" $ T.intercalate (T.pack "\n") input
    board = boardToGrid $ chunks !! 0
    path = segmentPath $ chunks !! 1
    (finalR, finalC, finalDir) = foldl
      (move board wraparound)
      (1, 51, 0)
      path
    finalScore = 1000 * finalR + 4 * finalC + finalDir

solve1 :: [Text] -> IO ()
solve1 input = print $ solve straightWraparound input

solve2 :: [Text] -> IO ()
solve2 input = print $ solve cubeWraparound input

segmentPath :: Text -> [String]
segmentPath path = map T.unpack $ go path T.empty []
  where
    go remaining current acc
      | T.null remaining = reverse (if T.null current then acc else current : acc)
      | first == 'L' || first == 'R' =
          let acc' = if T.null current then acc else current : acc
          in go (T.tail remaining) T.empty (T.singleton first : acc')
      | otherwise =
          go (T.tail remaining) (T.snoc current first) acc
      where
        first = T.head remaining

boardToGrid :: Text -> Map (Int, Int) Char
boardToGrid board =
  M.fromList
    [ ((r, c), ch)
    | (r, line) <- zip [1..] (T.lines board)
    , (c, ch) <- zip [1..] (T.unpack line)
    , ch /= ' '
    ]

moveStep :: Map (Int, Int) Char
  -> Map State State
  -> State
  -> Maybe State
moveStep board wraparound (r, c, dir) =
  if board M.! (r', c') == '#' then Nothing else Just st''
  where
    st' = case dir of
      3 -> (r - 1, c, dir)
      1 -> (r + 1, c, dir)
      2 -> (r, c - 1, dir)
      0 -> (r, c + 1, dir)
      _ -> error "invalid direction"
    st'' = fromMaybe st' $ M.lookup st' wraparound
    (r', c', _) = st''

moveSteps :: Map (Int, Int) Char
  -> Map State State
  -> State
  -> Int
  -> State
moveSteps _ _ st 0 = st
moveSteps board wraparound st n =
  case moveStep board wraparound st of
    Nothing -> st
    Just st' -> moveSteps board wraparound st' (n - 1)

move :: Map (Int, Int) Char
  -> Map State State
  -> State
  -> String
  -> State
move _ _ (r, c, dir) "L" = (r, c, (dir - 1) `mod` 4)
move _ _ (r, c, dir) "R" = (r, c, (dir + 1) `mod` 4)
move board wraparound st stepsStr = moveSteps board wraparound st $ read stepsStr
