module Day18(solve1, solve2) where
import Data.Set (Set)
import Data.Set qualified as S
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Utils.Misc (count)
import Utils

solve1 :: [Text] -> IO ()
solve1 input = do
  let cubes = map parseCube input
  let (_, surfaceArea) = foldl incrementArea (S.empty, 0) cubes
  print surfaceArea

solve2 :: [Text] -> IO ()
solve2 input = do
  let cubes = map parseCube input
  print (exposedArea cubes)

parseCube :: Text -> (Int, Int, Int)
parseCube t = (x, y, z)
  where
    [x, y, z] = map readT (splitT "," t) :: [Int]

incrementArea :: (Set (Int, Int, Int), Int) -> (Int, Int, Int) -> (Set (Int, Int, Int), Int)
incrementArea (existingCubes, sa) (x, y, z) = (S.insert (x, y, z) existingCubes, sa')
  where
    directions = [(1,0,0),(-1,0,0),(0,1,0),(0,-1,0),(0,0,1),(0,0,-1)]
    numOverlapping = sum $ map (\(dx, dy, dz) -> if S.member (x+dx, y+dy, z+dz) existingCubes then 1 else 0) directions
    sa' = sa + 6 - 2 * numOverlapping

neighbors3d :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbors3d (x, y, z) = [(x+1, y, z), (x-1, y, z), (x, y+1, z), (x, y-1, z), (x, y, z+1), (x, y, z-1)]

exposedArea :: [(Int, Int, Int)] -> Int
exposedArea cubes = foldr (\p acc -> acc + exposedSides p) 0 cubes
  where
    minX = minimum (map (\(x,_,_) -> x) cubes) - 1
    maxX = maximum (map (\(x,_,_) -> x) cubes) + 1
    minY = minimum (map (\(_,y,_) -> y) cubes) - 1
    maxY = maximum (map (\(_,y,_) -> y) cubes) + 1
    minZ = minimum (map (\(_,_,z) -> z) cubes) - 1
    maxZ = maximum (map (\(_,_,z) -> z) cubes) + 1
    cubeSet = S.fromList cubes
    inBox (x,y,z) = x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ
    start = (minX, minY, minZ)
    outsideAir = bfs (Seq.singleton start) (S.singleton start)
    bfs :: Seq (Int, Int, Int) -> Set (Int, Int, Int) -> Set (Int, Int, Int)
    bfs Seq.Empty seen = seen
    bfs (p Seq.:<| q) seen =
      let next = [ n | n <- neighbors3d p, inBox n, n `S.notMember` cubeSet, n `S.notMember` seen ]
          seen' = foldr S.insert seen next
          q' = q <> Seq.fromList next
      in bfs q' seen'
    exposedSides p = count (`S.member` outsideAir) (neighbors3d p)
