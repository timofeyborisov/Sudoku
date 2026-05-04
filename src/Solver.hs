module Solver
  ( solveStep
  , solveWithStrategy
  , solveFull
  , solverStrategies
  , strategyLabel
  ) where

import Data.List (minimumBy)
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set

import Board
import Types

-- Shared data
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

-- Labels
strategyLabel :: SolverStrategy -> String
strategyLabel StrategyNakedSingle = "NakedSingle"
strategyLabel StrategyHiddenSingle = "HiddenSingle"
strategyLabel StrategyNakedPair = "NakedPair"
strategyLabel StrategyBacktrack = "Backtrack"

solveStep :: Board -> Maybe (SolverStrategy, Cell, Digit)
solveStep = solveStepUpTo StrategyBacktrack

-- One step
solveStepUpTo :: SolverStrategy -> Board -> Maybe (SolverStrategy, Cell, Digit)
solveStepUpTo StrategyNakedSingle board =
  nakedSingle board
solveStepUpTo StrategyHiddenSingle board =
  firstJust
    [ nakedSingle board
    , hiddenSingle board
    ]
solveStepUpTo StrategyNakedPair board =
  firstJust
    [ nakedSingle board
    , hiddenSingle board
    , nakedPairStep board
    ]
solveStepUpTo StrategyBacktrack board =
  firstJust
    [ nakedSingle board
    , hiddenSingle board
    , nakedPairStep board
    , backtrackStep board
    ]

-- Full solve
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

-- Basic strategies
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
  listToMaybe (mapMaybe (hiddenSingleInUnit board) allUnits)

hiddenSingleInUnit :: Board -> LineKind -> Maybe (SolverStrategy, Cell, Digit)
hiddenSingleInUnit board lk =
  listToMaybe
    [ (StrategyHiddenSingle, cell, digit)
    | digit <- digits
    , let cells = possibleCellsFor board digit lk
    , [cell] <- [cells]
    ]

possibleCellsFor :: Board -> Digit -> LineKind -> [Cell]
possibleCellsFor board digit lk =
  [ cell
  | cell <- unitIndices lk
  , boardGet board cell == 0
  , digit `Set.member` candidatesAt board cell
  ]

nakedPairStep :: Board -> Maybe (SolverStrategy, Cell, Digit)
nakedPairStep board =
  listToMaybe (mapMaybe (nakedPairInUnit board) allUnits)

nakedPairInUnit :: Board -> LineKind -> Maybe (SolverStrategy, Cell, Digit)
nakedPairInUnit board lk =
  listToMaybe
    [ (StrategyNakedPair, cell, digit)
    | pair <- nakedPairs board lk
    , cell <- unitIndices lk
    , boardGet board cell == 0
    , candidatesAt board cell /= pair
    , let reduced = candidatesAt board cell `Set.difference` pair
    , Set.size reduced == 1
    , digit <- Set.toList reduced
    ]

nakedPairs :: Board -> LineKind -> [Set Digit]
nakedPairs board lk =
  [ pair
  | pair <- candidateSets
  , Set.size pair == 2
  , length (filter (== pair) candidateSets) == 2
  ]
  where
    candidateSets =
      [ candidatesAt board cell
      | cell <- unitIndices lk
      , boardGet board cell == 0
      ]

-- Backtracking
backtrackStep :: Board -> Maybe (SolverStrategy, Cell, Digit)
backtrackStep board = do
  (cell, candidates) <- bestBranchCell board
  digit <- listToMaybe
    [ candidate
    | candidate <- Set.toList candidates
    , isJust (solveWithStrategy StrategyBacktrack (boardSet board cell candidate))
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

-- Validation
hasContradiction :: Board -> Bool
hasContradiction board =
  not (Set.null (conflictingCells board)) ||
  any (Set.null . candidatesAt board) (emptyCells board)

-- Apply solution
applyMoves :: Board -> [(Cell, Digit)] -> Board
applyMoves = foldl boardAfterMove

boardAfterMove :: Board -> (Cell, Digit) -> Board
boardAfterMove board (cell, digit) = boardSet board cell digit

-- Small helper
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing : xs) = firstJust xs
firstJust (Just x : _) = Just x
