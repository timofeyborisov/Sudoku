module Events
  ( handleEvent
  , updateWorld
  ) where

import Graphics.Gloss.Interface.Pure.Game

import Types
import Board
import UI 

-- Processing of mouse or keyboard input
handleEvent :: Event -> World -> World
handleEvent event world
  | uiSolved (worldUI world) = world
  | otherwise = 
  case event of

    EventKey (MouseButton LeftButton) Down _ pos ->
      case pointToCell pos of
        Nothing -> world
        Just c  ->
          world
            { worldUI =
                (worldUI world)
                  { uiSelected = Just c
                  }
            }

    EventKey (Char ch) Down _ _ ->
      case (uiSelected (worldUI world), ch) of
        (Just cell, '0') ->
          clearCell world cell

        (Just cell, _) ->
          case charToDigit ch of
            Just digit -> applyInput world cell digit
            Nothing    -> world

        _ -> world

    EventKey key Down _ _ ->
      case (uiSelected (worldUI world), key) of
        (Just cell, SpecialKey KeyBackspace) ->
          clearCell world cell
        (Just cell, SpecialKey KeyDelete) ->
          clearCell world cell

        _ -> world

    _ -> world

updateWorld :: Float -> World -> World
updateWorld dt world =
  world { worldUI = updateUIEffects dt (worldUI world) }

updateUIEffects :: Float -> UIState -> UIState
updateUIEffects dt ui =
  ui
    { uiErrorAlpha  = newErrorAlpha
    , uiErrorCell   = newErrorCell
    , uiSolvedAlpha = newSolvedAlpha
    }
  where
    newErrorAlpha = max 0 (uiErrorAlpha ui - dt * 2.5)

    newErrorCell
      | newErrorAlpha <= 0 = Nothing
      | otherwise = uiErrorCell ui

    newSolvedAlpha
      | uiSolved ui = uiSolvedAlpha ui - dt * 0.05
      | otherwise   = 0.0

charToDigit :: Char -> Maybe Digit
charToDigit c
  | c >= '1' && c <= '9' = Just (Digit (read [c]))
  | otherwise = Nothing

applyInput :: World -> Cell -> Digit -> World
applyInput world cell digit =
  case applyMove (worldGame world) cell digit of
    Left err ->
      setErrorState world cell err

    Right newGame
      | isSolved (gsCurrent newGame) ->
          setSolvedState world newGame

      | otherwise ->
          setNormalState world newGame

-- Helper functions
setErrorState :: World -> Cell -> MoveValidity -> World
setErrorState world cell err =
  world
    { worldUI =
        (worldUI world)
          { uiMessage    = Just (prettyMoveValidity err)
          , uiErrorCell  = Just cell
          , uiErrorAlpha = 1.0
          }
    }

setSolvedState :: World -> GameState -> World
setSolvedState world newGame =
  world
    { worldGame = newGame
    , worldUI =
        (worldUI world)
          { uiSolved = True 
          , uiSelected = Nothing
          , uiHover = Nothing
          , uiMessage = Just "Solved!"
          , uiErrorCell = Nothing
          , uiErrorAlpha = 0.0
          , uiSolvedAlpha = 1.0
          }
    }

setNormalState :: World -> GameState -> World
setNormalState world newGame =
  world
    { worldGame = newGame
    , worldUI =
        (worldUI world)
          { uiSolved = False
          , uiMessage = Just "Sudoku"
          , uiErrorCell = Nothing
          , uiErrorAlpha = 0.0
          , uiSolvedAlpha = 0.0
          }
    }

clearCell :: World -> Cell -> World
clearCell world cell =
  let gs = worldGame world
   in world
        { worldGame = gs { gsCurrent = boardClear (gsCurrent gs) cell }
        , worldUI   = (worldUI world)
            { uiMessage   = Just "Sudoku"
            , uiErrorCell = Nothing
            }
        }
        
-- Print error messages
prettyMoveValidity :: MoveValidity -> String
prettyMoveValidity MoveOk = "Sudoku"
prettyMoveValidity MoveConflict = "This move is not allowed"
prettyMoveValidity MoveImmutable = "This cell cannot be changed"
prettyMoveValidity MoveInvalidDigit = "Only digits 1-9 are allowed"
prettyMoveValidity MoveInvalidCell = "Invalid cell"