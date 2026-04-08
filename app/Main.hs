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
main = do
  -- gen <- getStdGen
  -- puzzle <- loadRandomPuzzle gen
  puzzle <- loadPuzzleByIndex 0

  let world =
        case puzzle of
          Left err ->
            initialWorld
              { worldUI =
                  (worldUI initialWorld)
                    { uiMessage = Just ("Load error: " ++ err) }
              }

          Right loaded ->
            World
              { worldGame = gameFromLoaded loaded
              , worldUI   = initialUIState
              }
  
  play
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
    , worldUI   = initialUIState
    }

initialGameState :: GameState
initialGameState =
  GameState
    { gsInitial = emptyBoard
    , gsCurrent = emptyBoard
    , gsGivens  = V.replicate 81 False
    }

initialUIState :: UIState
initialUIState =
  UIState
    { uiSolved = False
    , uiSelected = Nothing
    , uiHover = Nothing
    , uiShowHints = False
    , uiShowConflicts = True
    , uiMessage = Just "Sudoku"
    , uiErrorCell = Nothing
    , uiErrorAlpha = 0.0
    , uiSolvedAlpha = 0.0
    }