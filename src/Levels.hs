module Levels
  ( levelFiles
  -- Loading
  , loadPuzzleFromFile
  , parsePuzzleString
  -- Picking puzzle
  , pickRandomPuzzlePath
  , loadRandomPuzzle
  , loadPuzzleByIndex
  ) where

import System.Random (StdGen, randomR)
import qualified Data.Vector as V
import Data.Char (isSpace)

import Types


levelFiles :: [FilePath]
levelFiles =
  [ "levels/Level1.txt"
  , "levels/Level2.txt"
  , "levels/Level3.txt"
  ]

-- 81 characters, empty cell — 0, "." or space.
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


pickRandomPuzzlePath :: [FilePath] -> StdGen -> (FilePath, StdGen)
pickRandomPuzzlePath paths g =
  case paths of
    [] -> error "pickRandomPuzzlePath: empty list"
    _ ->
      let (i, g') = randomR (0, length paths - 1) g
       in (paths !! i, g')

loadRandomPuzzle :: StdGen -> IO (Either String LoadedPuzzle)
loadRandomPuzzle g = do
  let paths = levelFiles
  if null paths
    then pure (Left "No level files found")
    else do
      let (path, _) = pickRandomPuzzlePath paths g
      loadPuzzleFromFile path

-- Load a puzzle by its index in the list for the given difficulty.
loadPuzzleByIndex :: Int -> IO (Either String LoadedPuzzle)
loadPuzzleByIndex idx
  | idx < 0 || idx >= length levelFiles =
      pure (Left "Index out of range")
  | otherwise =
      loadPuzzleFromFile (levelFiles !! idx)
