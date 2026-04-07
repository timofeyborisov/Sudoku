module Main where

import Graphics.Gloss
import qualified Data.Vector as V

import Types
import UI
import Events
import Config

main :: IO ()
main =
  play
    window
    backgroundColor
    fps
    initialWorld
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
    { uiSelected      = Nothing
    , uiHover         = Nothing
    , uiShowHints     = False
    , uiShowConflicts = True
    , uiMessage       = Just "Sudoku"
    }

emptyBoard :: Board
emptyBoard = Board (V.replicate 81 0)