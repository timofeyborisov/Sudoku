module Main where

import Graphics.Gloss
import qualified Data.Vector as V

import Types
import UI
import Events
import Config
import Levels
import Board
import Assets

-- App entry
main :: IO ()
main = do
  assets <- loadUIAssets
  let world =
        case builtInPuzzles of
          Left err ->
            (initialWorld assets)
              { worldUI =
                  initialUIState
                    { uiMessage = Just ("Load error: " ++ err)
                    }
              }

          Right puzzles ->
            (initialWorld assets)
              { worldLevels = puzzles
              , worldRandomSeed = 1357911
              , worldUI = initialUIState
              }

  play
    window
    backgroundColor
    fps
    world
    renderWorld
    handleEvent
    updateWorld

-- Window setup
window :: Display
window =
  InWindow
    windowTitle
    (windowWidth, windowHeight)
    (100, 80)

-- Initial state
initialWorld :: UIAssets -> World
initialWorld assets =
  World
    { worldGame = initialGameState
    , worldUI = initialUIState
    , worldLevels = []
    , worldRandomSeed = 1
    , worldAssets = assets
    }

initialGameState :: GameState
initialGameState =
  GameState
    { gsInitial = emptyBoard
    , gsCurrent = emptyBoard
    , gsGivens = V.replicate 81 False
    }

initialUIState :: UIState
initialUIState =
  UIState
    { uiScreen = MainMenu
    , uiTheme = LightTheme
    , uiSolved = False
    , uiSelected = Nothing
    , uiHoverCell = Nothing
    , uiHoverButton = Nothing
    , uiShowConflicts = True
    , uiMessage = Just "Sudoku"
    , uiErrorCell = Nothing
    , uiErrorAlpha = 0.0
    , uiSolvedAlpha = 0.0
    , uiSolveScript = []
    , uiSolveTimer = 0.0
    }
