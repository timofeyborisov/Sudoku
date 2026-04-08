module Types
  ( Digit(..)
  , Cell(..)
  , Board(..)
  , Difficulty(..)
  , MoveValidity(..)
  , LineKind(..)
  , SolverStrategy(..)
  , LoadedPuzzle(..)
  , GameState(..)
  , UIState(..)
  , World(..)
  ) where

import Data.Vector (Vector)

-- 1..9
newtype Digit = Digit Int
  deriving (Eq, Ord, Show, Read)

-- 0..80
newtype Cell = Cell Int
  deriving (Eq, Ord, Show, Read)

-- 9x9
newtype Board = Board (Vector Int)
  deriving (Eq, Show, Read)

data Difficulty
  = Easy
  | Medium
  | Hard
  | Expert
  | Extreme
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data MoveValidity
  = MoveOk
  | MoveConflict -- The move breaks a row, column, or 3x3 block.
  | MoveImmutable -- Given by the condition
  | MoveInvalidCell -- Out of 0..80
  deriving (Eq, Show)

data LineKind
  = Row Int
  | Col Int
  | Box Int -- 3x3 
  deriving (Eq, Show)

data SolverStrategy
  = StrategyNakedSingle -- There is only one candidate in a cell
  | StrategyHiddenSingle -- In a unit (row/column/block), a digit is only possible in one cell
  | StrategyNakedPair -- Reserved
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

data UIState = UIState
  { uiSolved :: Bool
  , uiSelected :: Maybe Cell
  , uiHover :: Maybe Cell
  , uiShowHints :: Bool
  , uiShowConflicts :: Bool
  , uiMessage :: Maybe String
  , uiErrorCell :: Maybe Cell
  , uiErrorAlpha :: Float
  , uiSolvedAlpha :: Float
  } deriving (Eq, Show)

data World = World
  { worldGame :: GameState
  , worldUI :: UIState
  } deriving (Eq, Show)
