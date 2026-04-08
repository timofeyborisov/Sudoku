module Levels
  ( 
  -- Paths to puzzle sets
    defaultLevelDir
  , levelFilesForDifficulty
  , minBundledPuzzleCount
  -- Loading
  , loadPuzzleFromFile
  , parsePuzzleString
  -- Puzzle choice
  , pickRandomPuzzlePath
  , loadRandomPuzzle
  , loadPuzzleByDifficulty
  ) where

import System.Random (StdGen, randomR)
import qualified Data.Vector as V
import Data.Char (isSpace)

import Types

-- Directory with level files relative to launch (replace with data-files if necessary)
defaultLevelDir :: FilePath
defaultLevelDir = "levels"

-- List of files for a given difficulty
levelFilesForDifficulty :: Difficulty -> [FilePath]
levelFilesForDifficulty Easy    = ["levels/Easy.txt"]
levelFilesForDifficulty Medium  = ["levels/Medium.txt"]
levelFilesForDifficulty Hard    = ["levels/Hard.txt"]
levelFilesForDifficulty Expert  = ["levels/Hard.txt"]
levelFilesForDifficulty Extreme = ["levels/Hard.txt"]

-- Minimum number of built-in puzzles
minBundledPuzzleCount :: Int
minBundledPuzzleCount = 5

-- Read file: 81 characters, empty cell — @0@, @.@ or space.
loadPuzzleFromFile :: FilePath -> IO (Either String LoadedPuzzle)
loadPuzzleFromFile path = do
  content <- readFile path
  case lines content of
    []      -> pure (Left "Empty file")
    (p : _) -> pure (parsePuzzleString p)

-- Parse normalized puzzle text of length 81.
parsePuzzleString :: String -> Either String LoadedPuzzle
parsePuzzleString input
  | length chars /= 81 = Left "Puzzle must contain exactly 81 cells"
  | otherwise =
      case traverse charToCell chars of
        Left err -> Left err
        Right values ->
          Right $
            LoadedPuzzle
              { lpBoard  = Board (V.fromList values)
              , lpGivens = V.fromList (map (/= 0) values)
              }
  where
    chars = filter (not . isSpace) input

    charToCell :: Char -> Either String Int
    charToCell '.' = Right 0
    charToCell '0' = Right 0
    charToCell c
      | c >= '1' && c <= '9' = Right (read [c])
      | otherwise = Left ("Invalid character: " ++ [c])



-- Random file picker
pickRandomPuzzlePath :: [FilePath] -> StdGen -> (FilePath, StdGen)
pickRandomPuzzlePath paths g =
  case paths of
    [] -> error "pickRandomPuzzlePath: empty list"
    _ ->
      let (i, g') = randomR (0, length paths - 1) g
       in (paths !! i, g')

-- Load a random puzzle of the given difficulty
loadRandomPuzzle :: Difficulty -> StdGen -> IO (Either String LoadedPuzzle)
loadRandomPuzzle diff g = do
  let paths = levelFilesForDifficulty diff
  if null paths
    then pure (Left "No files for this difficulty")
    else do
      let (path, _) = pickRandomPuzzlePath paths g
      loadPuzzleFromFile path

-- Load a puzzle by its index in the list for the given difficulty.
loadPuzzleByDifficulty :: Difficulty -> Int -> IO (Either String LoadedPuzzle)
loadPuzzleByDifficulty diff idx =
  case drop idx (levelFilesForDifficulty diff) of
    []      -> pure (Left "Index out of range")
    (p : _) -> loadPuzzleFromFile p
