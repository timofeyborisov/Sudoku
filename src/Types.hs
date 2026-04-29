module Types
  ( Digit(..)
  , Cell(..)
  , Board(..)
  , Difficulty(..)
  , MoveValidity(..)
  , LineKind(..)
  , SolverStrategy(..)
  , Screen(..)
  , ButtonAction(..)
  , SolveAction(..)
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
  = StrategyNakedSingle
  | StrategyHiddenSingle
  | StrategyNakedPair
  | StrategyBacktrack
  deriving (Eq, Show, Enum, Bounded)

data Screen
  = MainMenu
  | DifficultyMenu
  | PuzzleMenu Difficulty
  | Playing
  deriving (Eq, Show)

data ButtonAction
  = ButtonPlay
  | ButtonDifficulty Difficulty
  | ButtonPuzzle Int
  | ButtonRandom
  | ButtonBack
  | ButtonHint
  | ButtonSolve
  | ButtonMenu
  deriving (Eq, Show)

data SolveAction
  = SolveStatus String Float
  | SolveMove Cell Digit
  | SolveFinish String
  deriving (Eq, Show)

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
  { uiScreen :: Screen
  , uiSolved :: Bool
  , uiSelected :: Maybe Cell
  , uiShowConflicts :: Bool
  , uiMessage :: Maybe String
  , uiErrorCell :: Maybe Cell
  , uiErrorAlpha :: Float
  , uiSolvedAlpha :: Float
  , uiSolveScript :: [SolveAction]
  , uiSolveTimer :: Float
  } deriving (Eq, Show)

data World = World
  { worldGame :: GameState
  , worldUI :: UIState
  , worldLevels :: [(Difficulty, [LoadedPuzzle])]
  , worldRandomSeed :: Int
  } deriving (Eq, Show)
