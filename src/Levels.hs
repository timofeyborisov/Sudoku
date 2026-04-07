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

import Types

-- Directory with level files relative to launch (replace with data-files if necessary)
defaultLevelDir :: FilePath
defaultLevelDir = "levels"

-- List of files for a given difficulty
levelFilesForDifficulty :: Difficulty -> [FilePath]
levelFilesForDifficulty = undefined

-- Minimum number of built-in puzzles
minBundledPuzzleCount :: Int
minBundledPuzzleCount = 5

-- Read file: 81 characters, empty cell — @0@, @.@ or space.
loadPuzzleFromFile :: FilePath -> IO (Either String LoadedPuzzle)
loadPuzzleFromFile = undefined

-- Parse normalized puzzle text of length 81.
parsePuzzleString :: String -> Either String LoadedPuzzle
parsePuzzleString = undefined

pickRandomPuzzlePath :: [FilePath] -> StdGen -> (FilePath, StdGen)
pickRandomPuzzlePath paths g =
  case paths of
    [] -> error "pickRandomPuzzlePath: empty list"
    _ ->
      let (i, g') = randomR (0, length paths - 1) g
       in (paths !! i, g')

-- Load a random puzzle of the given difficulty
loadRandomPuzzle :: Difficulty -> StdGen -> IO (Either String LoadedPuzzle)
loadRandomPuzzle = undefined

-- Load a puzzle by its index in the list for the given difficulty.
loadPuzzleByDifficulty :: Difficulty -> Int -> IO (Either String LoadedPuzzle)
loadPuzzleByDifficulty = undefined
