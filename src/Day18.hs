module Day18 (solve1, solve2) where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Utils.Misc (count)
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let cubes = Set.fromList $ map parseCube input
  print $ surfaceArea cubes

solve2 :: [Text] -> IO ()
solve2 input = do
  let cubes = Set.fromList $ map parseCube input
  print $ exposedArea cubes

parseCube :: Text -> (Int, Int, Int)
parseCube t = (x, y, z)
  where
    [x, y, z] = map readT (splitT "," t) :: [Int]

neighbors3d :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbors3d (x, y, z) = [(x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z + 1), (x, y, z - 1)]

surfaceArea :: Set (Int, Int, Int) -> Int
surfaceArea cubes = Set.foldr (\p -> (+ exposedSides p)) 0 cubes
  where
    exposedSides p = count (`Set.notMember` cubes) (neighbors3d p)

exposedArea :: Set (Int, Int, Int) -> Int
exposedArea cubes = Set.foldr (\p -> (+ exposedSides p)) 0 cubes
  where
    minX = minimum (Set.map (\(x, _, _) -> x) cubes) - 1
    maxX = maximum (Set.map (\(x, _, _) -> x) cubes) + 1
    minY = minimum (Set.map (\(_, y, _) -> y) cubes) - 1
    maxY = maximum (Set.map (\(_, y, _) -> y) cubes) + 1
    minZ = minimum (Set.map (\(_, _, z) -> z) cubes) - 1
    maxZ = maximum (Set.map (\(_, _, z) -> z) cubes) + 1
    inBox (x, y, z) = x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ
    start = (minX, minY, minZ)
    outsideAir = bfs (Seq.singleton start) (Set.singleton start)
    bfs :: Seq (Int, Int, Int) -> Set (Int, Int, Int) -> Set (Int, Int, Int)
    bfs Seq.Empty seen = seen
    bfs (p Seq.:<| q) seen =
      let next = [n | n <- neighbors3d p, inBox n, n `Set.notMember` cubes, n `Set.notMember` seen]
          seen' = foldr Set.insert seen next
          q' = q Seq.>< Seq.fromList next
       in bfs q' seen'
    exposedSides p = count (`Set.member` outsideAir) (neighbors3d p)
