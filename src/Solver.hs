module Solver
  ( solveStep
  , solveWithStrategy
  , solveFull
  , solverStrategies
  , strategyLabel
  ) where

import Data.List (minimumBy)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set

import Board
import Types


digits :: [Digit]
digits = [Digit d | d <- [1 .. 9]]

allUnits :: [LineKind]
allUnits =
  [ Row i | i <- [0 .. 8] ] ++
  [ Col i | i <- [0 .. 8] ] ++
  [ Box i | i <- [0 .. 8] ]

solverStrategies :: [SolverStrategy]
solverStrategies =
  [ StrategyNakedSingle
  , StrategyHiddenSingle
  , StrategyNakedPair
  , StrategyBacktrack
  ]

strategyLabel :: SolverStrategy -> String
strategyLabel StrategyNakedSingle = "NakedSingle"
strategyLabel StrategyHiddenSingle = "HiddenSingle"
strategyLabel StrategyNakedPair = "NakedPair"
strategyLabel StrategyBacktrack = "Backtrack"

solveStep :: Board -> Maybe (SolverStrategy, Cell, Digit)
solveStep = solveStepUpTo StrategyBacktrack

solveStepUpTo :: SolverStrategy -> Board -> Maybe (SolverStrategy, Cell, Digit)
solveStepUpTo strategy board =
  firstJust (stepFunctions strategy board)

solveWithStrategy :: SolverStrategy -> Board -> Maybe [(Cell, Digit)]
solveWithStrategy strategy board
  | hasContradiction board = Nothing
  | isSolved board = Just []
  | otherwise = do
      (_, cell, digit) <- solveStepUpTo strategy board
      rest <- solveWithStrategy strategy (boardSet board cell digit)
      pure ((cell, digit) : rest)

solveFull :: Board -> Maybe Board
solveFull board =
  applyMoves board <$> solveWithStrategy StrategyBacktrack board

stepFunctions :: SolverStrategy -> Board -> [Maybe (SolverStrategy, Cell, Digit)]
stepFunctions StrategyNakedSingle board =
  [nakedSingle board]
stepFunctions StrategyHiddenSingle board =
  [ nakedSingle board
  , hiddenSingle board
  ]
stepFunctions StrategyNakedPair board =
  [ nakedSingle board
  , hiddenSingle board
  , nakedPairStep board
  ]
stepFunctions StrategyBacktrack board =
  [ nakedSingle board
  , hiddenSingle board
  , nakedPairStep board
  , backtrackStep board
  ]

nakedSingle :: Board -> Maybe (SolverStrategy, Cell, Digit)
nakedSingle board =
  listToMaybe
    [ (StrategyNakedSingle, cell, digit)
    | cell <- emptyCells board
    , let candidates = Set.toList (candidatesAt board cell)
    , [digit] <- [candidates]
    ]

hiddenSingle :: Board -> Maybe (SolverStrategy, Cell, Digit)
hiddenSingle board =
  listToMaybe (mapMaybe hiddenSingleInUnit allUnits)
  where
    hiddenSingleInUnit lk =
      listToMaybe
        [ (StrategyHiddenSingle, cell, digit)
        | digit <- digits
        , let cells = possibleCellsFor digit lk
        , [cell] <- [cells]
        ]

    possibleCellsFor digit lk =
      [ cell
      | cell <- unitIndices lk
      , boardGet board cell == 0
      , digit `Set.member` candidatesAt board cell
      ]

nakedPairStep :: Board -> Maybe (SolverStrategy, Cell, Digit)
nakedPairStep board =
  listToMaybe (mapMaybe nakedPairInUnit allUnits)
  where
    nakedPairInUnit lk =
      listToMaybe
        [ (StrategyNakedPair, cell, digit)
        | pair <- nakedPairs lk
        , cell <- unitIndices lk
        , boardGet board cell == 0
        , candidatesAt board cell /= pair
        , let reduced = candidatesAt board cell `Set.difference` pair
        , Set.size reduced == 1
        , digit <- Set.toList reduced
        ]

    nakedPairs lk =
      [ pair
      | let candidateSets =
              [ candidatesAt board cell
              | cell <- unitIndices lk
              , boardGet board cell == 0
              ]
      , pair <- candidateSets
      , Set.size pair == 2
      , length (filter (== pair) candidateSets) == 2
      ]

backtrackStep :: Board -> Maybe (SolverStrategy, Cell, Digit)
backtrackStep board = do
  (cell, candidates) <- bestBranchCell board
  digit <- listToMaybe
    [ candidate
    | candidate <- Set.toList candidates
    , solveWithStrategy StrategyBacktrack (boardSet board cell candidate) /= Nothing
    ]
  pure (StrategyBacktrack, cell, digit)

bestBranchCell :: Board -> Maybe (Cell, Set Digit)
bestBranchCell board =
  case cellsWithCandidates of
    [] -> Nothing
    xs -> Just (minimumBy (comparing (Set.size . snd)) xs)
  where
    cellsWithCandidates =
      [ (cell, candidatesAt board cell)
      | cell <- emptyCells board
      ]

emptyCells :: Board -> [Cell]
emptyCells board =
  [ cell
  | cell <- allCells
  , boardGet board cell == 0
  ]

hasContradiction :: Board -> Bool
hasContradiction board =
  not (Set.null (conflictingCells board)) ||
  any (Set.null . candidatesAt board) (emptyCells board)

applyMoves :: Board -> [(Cell, Digit)] -> Board
applyMoves = foldl applyOneMove
  where
    applyOneMove acc (cell, digit) = boardSet acc cell digit

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing : xs) = firstJust xs
firstJust (Just x : _) = Just x
