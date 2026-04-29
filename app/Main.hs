module Main where

import Graphics.Gloss
import qualified Data.Vector as V

import Types
import UI
import Events
import Config
import Levels
import Board

main :: IO ()
main =
  let world =
        case builtInPuzzles of
          Left err ->
            initialWorld
              { worldUI =
                  initialUIState
                    { uiMessage = Just ("Load error: " ++ err)
                    }
              }

          Right puzzles ->
            initialWorld
              { worldLevels = puzzles
              , worldRandomSeed = 1357911
              , worldUI = initialUIState
              }
  
   in play
        window
        backgroundColor
        fps
        world
        renderWorld
        handleEvent
        updateWorld

window :: Display
window =
  InWindow
    windowTitle
    (windowWidth, windowHeight)
    (100, 80)

initialWorld :: World
initialWorld =
  World
    { worldGame = initialGameState
    , worldUI = initialUIState
    , worldLevels = []
    , worldRandomSeed = 1
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
    , uiSolved = False
    , uiSelected = Nothing
    , uiShowConflicts = True
    , uiMessage = Just "Sudoku"
    , uiErrorCell = Nothing
    , uiErrorAlpha = 0.0
    , uiSolvedAlpha = 0.0
    , uiSolveScript = []
    , uiSolveTimer = 0.0
    }
