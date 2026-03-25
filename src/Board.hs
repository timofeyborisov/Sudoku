module Board
  ( -- Indexing
    cellToRC
  , rcToCell
  , rowOfCell
  , colOfCell
  , boxOfCell
  , rowIndices
  , colIndices
  , boxIndices
  , unitIndices
  , allCells
  -- Board
  , emptyBoard
  , boardGet
  , boardSet
  , boardClear
  -- Rules
  , candidatesAt
  , checkMove
  , hasConflictAt
  , conflictingCells
  , isUnitCompleteAndValid
  , isSolved
  -- Game
  , applyMove
  , gameFromLoaded
  -- Solvers
  , solveStep
  , solveFull
  ) where

import Data.Set (Set)
import Data.Vector (Vector)

import Types


-- Converters (RC - row and column of the cell)
cellToRC :: Cell -> (Int, Int)
cellToRC = undefined

rcToCell :: Int -> Int -> Cell
rcToCell = undefined

rowOfCell :: Cell -> Int
rowOfCell = undefined

colOfCell :: Cell -> Int
colOfCell = undefined

boxOfCell :: Cell -> Int
boxOfCell = undefined


-- All indicies of the defined lineKind
rowIndices :: Int -> [Cell]
rowIndices = undefined

colIndices :: Int -> [Cell]
colIndices = undefined

boxIndices :: Int -> [Cell]
boxIndices = undefined

unitIndices :: LineKind -> [Cell]
unitIndices = undefined

allCells :: [Cell]
allCells = undefined


-- Board processing

-- Define 9x9 board
emptyBoard :: Board
emptyBoard = undefined

-- Get cell value
boardGet :: Board -> Cell -> Int
boardGet = undefined

-- Preliminary check is assumed!
boardSet :: Board -> Cell -> Digit -> Board
boardSet = undefined

boardClear :: Board -> Cell -> Board
boardClear = undefined


-- Conflicts 
-- Set of candidates (what can be put in the cell)
candidatesAt :: Board -> Cell -> Set Digit
candidatesAt = undefined

checkMove :: Board -> Vector Bool -> Cell -> Digit -> MoveValidity
checkMove = undefined

hasConflictAt :: Board -> Cell -> Digit -> Bool
hasConflictAt = undefined

conflictingCells :: Board -> Set Cell
conflictingCells = undefined

isUnitCompleteAndValid :: Board -> LineKind -> Bool
isUnitCompleteAndValid = undefined

isSolved :: Board -> Bool
isSolved = undefined


-- Application
applyMove :: GameState -> Cell -> Digit -> Either MoveValidity GameState
applyMove = undefined

gameFromLoaded :: LoadedPuzzle -> GameState
gameFromLoaded = undefined


-- Solve 
solveStep :: Board -> Maybe (SolverStrategy, Cell, Digit)
solveStep = undefined

solveFull :: Board -> Maybe Board
solveFull = undefined
