import Data.List
import Data.Map qualified as Map
import Data.Text qualified as T
import Utils

data Dirent = File Int | Directory (Map.Map T.Text Dirent)

-- For debugging
instance Show Dirent where
  show (Directory subDir) = Map.foldlWithKey join "" subDir
    where
      join :: String -> T.Text -> Dirent -> String
      join acc name dirent =
        acc ++ "- " ++ T.unpack name
          ++ ( case dirent of
                 File size -> " (" ++ show size ++ ")\n"
                 Directory _ -> ":\n" ++ (unlines $ map ("  " ++) $ lines $ show dirent)
             )

type TraverseState = (Dirent, [T.Text])

main = do
  input <- getInput
  let logs = T.splitOn (T.pack "\n$ ") (T.drop 2 input)
  let (dir, path) = foldl' (flip processCmd) (Directory Map.empty, []) logs
  let dirSizes = sizes dir
  let total = head dirSizes
  print $ sum $ filter (<= 100000) dirSizes
  printMaybe $ find (>= total - 40000000) (sort dirSizes)

printMaybe :: Show a => Maybe a -> IO ()
printMaybe a = case a of
  Just a -> print a
  Nothing -> print "Nothing"

processCmd :: T.Text -> TraverseState -> TraverseState
processCmd log
  | command == "cd" = cd (T.drop 3 log)
  | command == "ls" = ls (tail $ T.lines log)
  where
    command = T.unpack $ T.take 2 log

cd :: T.Text -> TraverseState -> TraverseState
cd dir (tree, path)
  | dir == T.pack ".." = (tree, tail path)
  | dir == T.pack "/" = (tree, [])
  | otherwise = (tree, dir : path)

data ParsedEntry = FileEntry T.Text Int | DirEntry T.Text deriving (Show)

ls :: [T.Text] -> TraverseState -> TraverseState
ls logs (tree, path) = (newTree, path)
  where
    newTree = updateSubtree (reverse path) tree $ Directory $ addEntries $ getDirents path tree

    getDirents :: [T.Text] -> Dirent -> Map.Map T.Text Dirent
    getDirents [] (Directory root) = root
    getDirents (dirName : parentPath) dir = case Map.lookup dirName (getDirents parentPath dir) of
      Just (Directory subDir) -> subDir
      _ -> error ("Directory not found: " ++ T.unpack dirName)

    parseLsLog :: T.Text -> ParsedEntry
    parseLsLog log
      | size == T.pack "dir" = DirEntry name
      | otherwise = FileEntry name (readT size)
      where
        [size, name] = T.words log

    addEntries :: Map.Map T.Text Dirent -> Map.Map T.Text Dirent
    addEntries dirents = foldr addEntry dirents $ map parseLsLog logs
      where
        addEntry (FileEntry name size) dirents = Map.insert name (File size) dirents
        addEntry (DirEntry name) dirents = case Map.lookup name dirents of
          Just subDir -> dirents
          _ -> Map.insert name (Directory Map.empty) dirents

    -- 2nd argument is the root; 3rd is the subtree to be inserted; 1st is its path
    updateSubtree :: [T.Text] -> Dirent -> Dirent -> Dirent
    updateSubtree [] _ newDir = newDir
    updateSubtree (topSeg : rest) (Directory root) newDir =
      Directory
        ( Map.insert topSeg (updateSubtree rest subtree newDir) root
        )
      where
        subtree = case Map.lookup topSeg root of
          Just dir -> dir
          Nothing -> error ("Directory not found: " ++ T.unpack topSeg)

sizes :: Dirent -> [Int]
sizes (Directory subDir) = (subdirsSize + filesSize) : (concat allSizes)
  where
    (files, subdirs) = partition isFile $ Map.elems subDir
    allSizes = map sizes subdirs
    subdirsSize = sumMap head allSizes
    filesSize = sumMap (\(File size) -> size) files

    isFile :: Dirent -> Bool
    isFile (File _) = True
    isFile _ = False
