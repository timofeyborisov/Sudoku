module Types
  ( Digit(..)
  , Cell(..)
  , Board(..)
  , Difficulty(..)
  , MoveValidity(..)
  , LineKind(..)
  , SolverStrategy(..)
  , Theme(..)
  , Screen(..)
  , ButtonAction(..)
  , SolveAction(..)
  , UIAssets(..)
  , LoadedPuzzle(..)
  , GameState(..)
  , UIState(..)
  , World(..)
  ) where

import Data.Vector (Vector)
import Graphics.Gloss.Data.Picture (Picture)

-- Board values
newtype Digit = Digit Int
  deriving (Eq, Ord, Show, Read)

newtype Cell = Cell Int
  deriving (Eq, Ord, Show, Read)

newtype Board = Board (Vector Int)
  deriving (Eq, Show, Read)

-- Game settings
data Difficulty
  = Easy
  | Medium
  | Hard
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- Move results
data MoveValidity
  = MoveOk
  | MoveConflict
  | MoveImmutable
  | MoveInvalidCell
  deriving (Eq, Show)

-- Board groups
data LineKind
  = Row Int
  | Col Int
  | Box Int
  deriving (Eq, Show)

-- Solver modes
data SolverStrategy
  = StrategyNakedSingle
  | StrategyHiddenSingle
  | StrategyNakedPair
  | StrategyBacktrack
  deriving (Eq, Show, Enum, Bounded)

-- Visual theme
data Theme
  = LightTheme
  | DarkTheme
  deriving (Eq, Show)

-- App screens
data Screen
  = MainMenu
  | DifficultyMenu
  | PuzzleMenu Difficulty
  | Playing
  deriving (Eq, Show)

-- Button actions
data ButtonAction
  = ButtonPlay
  | ButtonDifficulty Difficulty
  | ButtonPuzzle Int
  | ButtonRandom
  | ButtonGenerate
  | ButtonBack
  | ButtonHint
  | ButtonSolve
  | ButtonTheme
  | ButtonReset
  | ButtonMenu
  deriving (Eq, Show)

-- Solver animation
data SolveAction
  = SolveStatus String Float
  | SolveMove Cell Digit
  | SolveFinish String
  deriving (Eq, Show)

-- UI icons
data UIAssets = UIAssets
  { uiBulbIcon :: Picture
  , uiBulbIconWhite :: Picture
  , uiThemeIcon :: Picture
  , uiThemeIconWhite :: Picture
  , uiDiceIcon :: Picture
  , uiDiceIconWhite :: Picture
  , uiResetIcon :: Picture
  , uiResetIconWhite :: Picture
  , uiGenerateIcon :: Picture
  , uiGenerateIconWhite :: Picture
  }

-- Loaded puzzle
data LoadedPuzzle = LoadedPuzzle
  { lpBoard :: Board
  , lpGivens :: Vector Bool
  } deriving (Eq, Show)

-- Game state
data GameState = GameState
  { gsInitial :: Board
  , gsCurrent :: Board
  , gsGivens :: Vector Bool
  } deriving (Eq, Show)

-- UI state
data UIState = UIState
  { uiScreen :: Screen
  , uiTheme :: Theme
  , uiSolved :: Bool
  , uiSelected :: Maybe Cell
  , uiHoverCell :: Maybe Cell
  , uiHoverButton :: Maybe ButtonAction
  , uiShowConflicts :: Bool
  , uiMessage :: Maybe String
  , uiErrorCell :: Maybe Cell
  , uiErrorAlpha :: Float
  , uiSolvedAlpha :: Float
  , uiSolveScript :: [SolveAction]
  , uiSolveTimer :: Float
  } deriving (Eq, Show)

-- Whole app state
data World = World
  { worldGame :: GameState
  , worldUI :: UIState
  , worldLevels :: [(Difficulty, [LoadedPuzzle])]
  , worldRandomSeed :: Int
  , worldAssets :: UIAssets
  }
