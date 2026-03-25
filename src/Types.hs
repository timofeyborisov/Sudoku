module Types
  ( Digit
  , Cell
  , Board
  , Difficulty (..)
  , MoveValidity (..)
  , LineKind (..)
  , SolverStrategy (..)
  , LoadedPuzzle (..)
  , GameState (..)
  ) where

import Data.Vector (Vector)

-- 1..9
type Digit = Int

-- 0..80
type Cell = Int

-- 9x9
type Board = Vector Int

data Difficulty
  = Easy
  | Medium
  | Hard
  | Expert
  | Extreme
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- Is move correct?
data MoveValidity
  = MoveOk
  | MoveConflict -- The move breaks a row, column, or 3x3 block.
  | MoveImmutable -- Given by the condition
  | MoveInvalidDigit -- Out of 1..9
  | MoveInvalidCell -- Out of 0..80
  deriving (Eq, Show)

data LineKind
  = Row Int
  | Col Int
  | Box Int -- Block
  deriving (Eq, Show)

data SolverStrategy
  = StrategyNakedSingle -- There is only one candidate in a cell
  | StrategyHiddenSingle -- In a unit (row/column/block), a digit is only possible in one cell
  | StrategyNakedPair -- Reserved for further heuristics
  | StrategyBacktrack -- Brute force with backtracking if there are no simple moves
  deriving (Eq, Show, Enum, Bounded)

data LoadedPuzzle = LoadedPuzzle
  { lpBoard :: Board
  , lpGivens :: Vector Bool
  } deriving (Eq, Show)

data GameState = GameState
  { gsInitial :: Board
  , gsCurrent :: Board
  , gsGivens :: Vector Bool
  } deriving (Eq, Show)
