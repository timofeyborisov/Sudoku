module Generator
  ( generatePuzzle
  ) where

import Data.Char (digitToInt)
import Data.Maybe (isJust, isNothing)
import qualified Data.Vector as V
import System.Random (StdGen, randomR)

import Board
import Levels (builtInPuzzles)
import Solver (solveWithStrategy)
import Types

-- Main generator
generatePuzzle :: Difficulty -> StdGen -> (LoadedPuzzle, StdGen)
generatePuzzle difficulty gen =
  case difficultyPuzzles difficulty of
    [] -> (defaultPuzzle, gen)
    puzzles ->
      let (basePuzzle, gen1) = pickRandom gen puzzles
          (digitOrder, gen2) = shuffleList gen1 [1 .. 9]
          (rowOrder, gen3) = shuffledIndices gen2
          (colOrder, gen4) = shuffledIndices gen3
       in (transformPuzzle digitOrder rowOrder colOrder basePuzzle, gen4)

-- Difficulty filter
difficultyPuzzles :: Difficulty -> [LoadedPuzzle]
difficultyPuzzles difficulty =
  case builtInPuzzles of
    Left _ -> []
    Right groups ->
      let allPuzzles = concatMap snd groups
          exactMatches = filter (matchesDifficulty difficulty . lpBoard) allPuzzles
       in
        case exactMatches of
          [] ->
            case lookup difficulty groups of
              Just puzzles -> puzzles
              Nothing -> []
          puzzles -> puzzles

matchesDifficulty :: Difficulty -> Board -> Bool
matchesDifficulty Easy board =
  isJust (solveWithStrategy StrategyNakedSingle board)
matchesDifficulty Medium board =
  isNothing (solveWithStrategy StrategyNakedSingle board) &&
  isJust (solveWithStrategy StrategyHiddenSingle board)
matchesDifficulty Hard board =
  isNothing (solveWithStrategy StrategyHiddenSingle board) &&
  isJust (solveWithStrategy StrategyBacktrack board)

-- Puzzle transforms
transformPuzzle :: [Int] -> [Int] -> [Int] -> LoadedPuzzle -> LoadedPuzzle
transformPuzzle digitOrder rowOrder colOrder puzzle =
  LoadedPuzzle
    { lpBoard = Board (V.fromList boardValues)
    , lpGivens = V.fromList givenValues
    }
  where
    boardValues =
      [ transformedValue row col
      | row <- [0 .. 8]
      , col <- [0 .. 8]
      ]

    givenValues =
      [ originalGiven row col
      | row <- [0 .. 8]
      , col <- [0 .. 8]
      ]

    transformedValue row col =
      remapDigit digitOrder (originalPuzzleValue puzzle rowOrder colOrder row col)

    originalGiven row col =
      originalPuzzleGiven puzzle rowOrder colOrder row col

cellIndex :: Int -> Int -> Int
cellIndex row col = row * 9 + col

originalPuzzleValue :: LoadedPuzzle -> [Int] -> [Int] -> Int -> Int -> Int
originalPuzzleValue puzzle rowOrder colOrder row col =
  boardGet (lpBoard puzzle) (rcToCell (rowOrder !! row) (colOrder !! col))

originalPuzzleGiven :: LoadedPuzzle -> [Int] -> [Int] -> Int -> Int -> Bool
originalPuzzleGiven puzzle rowOrder colOrder row col =
  lpGivens puzzle V.! cellIndex (rowOrder !! row) (colOrder !! col)

remapDigit :: [Int] -> Int -> Int
remapDigit _ 0 = 0
remapDigit digitOrder value = digitOrder !! (value - 1)

-- Random permutations
shuffledIndices :: StdGen -> ([Int], StdGen)
shuffledIndices gen =
  let (bands, gen1) = shuffleList gen [0, 1, 2]
      (rows0, gen2) = shuffleList gen1 [0, 1, 2]
      (rows1, gen3) = shuffleList gen2 [0, 1, 2]
      (rows2, gen4) = shuffleList gen3 [0, 1, 2]
      rowGroups = [rows0, rows1, rows2]
      expand band = map (+ band * 3) (rowGroups !! band)
   in (concatMap expand bands, gen4)

pickRandom :: StdGen -> [a] -> (a, StdGen)
pickRandom gen xs =
  let (index, gen') = randomR (0, length xs - 1) gen
   in (xs !! index, gen')

-- List shuffle
shuffleList :: StdGen -> [a] -> ([a], StdGen)
shuffleList gen [] = ([], gen)
shuffleList gen xs =
  let (index, gen1) = randomR (0, length xs - 1) gen
   in
    case removeAt index xs of
      Nothing -> (xs, gen1)
      Just (picked, rest) ->
        let (shuffledRest, gen2) = shuffleList gen1 rest
         in (picked : shuffledRest, gen2)

removeAt :: Int -> [a] -> Maybe (a, [a])
removeAt _ [] = Nothing
removeAt 0 (x : xs) = Just (x, xs)
removeAt index (x : xs) =
  do
    (picked, rest) <- removeAt (index - 1) xs
    pure (picked, x : rest)

-- Fallback puzzle
defaultPuzzle :: LoadedPuzzle
defaultPuzzle =
  loadedPuzzleFromString
    "530070000600195000098000060800060003400803001700020006060000280000419005000080079"

loadedPuzzleFromString :: String -> LoadedPuzzle
loadedPuzzleFromString raw =
  LoadedPuzzle
    { lpBoard = Board (V.fromList values)
    , lpGivens = V.fromList (map (/= 0) values)
    }
  where
    values = map charToValue raw

charToValue :: Char -> Int
charToValue '0' = 0
charToValue c = digitToInt c
