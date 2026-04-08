module UI
  ( renderWorld
  , drawGrid
  , drawBoard
  , drawDigits
  , drawSelection
  , drawConflicts
  , drawStatusBar
  , cellCenter
  , cellRect
  , pointToCell
  , withAlpha
  ) where

import Graphics.Gloss
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

import Types
import Board
import Config

-- Geometry
boardSize :: Float
boardSize = 9 * cellSize

boardLeft :: Float
boardLeft = -(boardSize / 2)

boardTop :: Float
boardTop = boardSize / 2

statusY :: Float
statusY = -320

thinLineWidth :: Float
thinLineWidth = 1

thickLineWidth :: Float
thickLineWidth = 3

-- Main render
renderWorld :: World -> Picture
renderWorld world =
  Pictures
    [ drawBackground
    , drawSelection world
    , drawErrorCell world
    , drawSolvedOverlay world
    , drawConflicts world
    , drawGrid
    , drawBoard world
    , drawStatusBar world
    ]

-- Background
drawBackground :: Picture
drawBackground =
  Color backgroundColor $
    rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)

-- Grid
thinLineIndices :: [Int]
thinLineIndices = [1, 2, 4, 5, 7, 8]

thickInnerLineIndices :: [Int]
thickInnerLineIndices = [3, 6]

xAt :: Int -> Float
xAt i = boardLeft + fromIntegral i * cellSize

yAt :: Int -> Float
yAt i = boardTop - fromIntegral i * cellSize

drawGrid :: Picture
drawGrid =
  Pictures
    ( thinVertical
   ++ thinHorizontal
   ++ thickVerticalInner
   ++ thickHorizontalInner
   ++ outerFrame
    )
  where
    centerX = boardLeft + boardSize / 2
    centerY = boardTop - boardSize / 2

    thinVertical =
      [ Color thinGridColor $
          translate (xAt i) centerY $
            rectangleSolid thinLineWidth boardSize
      | i <- thinLineIndices
      ]

    thinHorizontal =
      [ Color thinGridColor $
          translate centerX (yAt i) $
            rectangleSolid boardSize thinLineWidth
      | i <- thinLineIndices
      ]

    thickVerticalInner =
      [ Color thickGridColor $
          translate (xAt i) centerY $
            rectangleSolid thickLineWidth (boardSize - thickLineWidth)
      | i <- thickInnerLineIndices
      ]

    thickHorizontalInner =
      [ Color thickGridColor $
          translate centerX (yAt i) $
            rectangleSolid (boardSize - thickLineWidth) thickLineWidth
      | i <- thickInnerLineIndices
      ]

    outerFrame =
      [ Color thickGridColor $
          translate centerX (yAt 0) $
            rectangleSolid (boardSize + thickLineWidth) thickLineWidth

      , Color thickGridColor $
          translate centerX (yAt 9) $
            rectangleSolid (boardSize + thickLineWidth) thickLineWidth

      , Color thickGridColor $
          translate (xAt 0) centerY $
            rectangleSolid thickLineWidth boardSize

      , Color thickGridColor $
          translate (xAt 9) centerY $
            rectangleSolid thickLineWidth boardSize
      ]

-- Board
drawBoard :: World -> Picture
drawBoard = drawDigits

drawDigits :: World -> Picture
drawDigits world =
  Pictures
    [ drawDigitAt world c
    | c <- allCells
    ]

drawDigitAt :: World -> Cell -> Picture
drawDigitAt world c
  | value == 0 = Blank
  | otherwise =
      translate x y $
        scale 0.22 0.22 $
          Color digitColor $
            Text (show value)
  where
    gs = worldGame world
    value = boardGet (gsCurrent gs) c

    (x0, y0) = cellCenter c
    x = x0 - 9
    y = y0 - 14

    digitColor =
      if isGiven gs c then givenDigitColor else userDigitColor

isGiven :: GameState -> Cell -> Bool
isGiven gs (Cell i) = gsGivens gs V.! i

-- Selection
drawSelection :: World -> Picture
drawSelection world =
  case uiSelected (worldUI world) of
    Nothing -> Blank
    Just c  ->
      let (x, y, w, h) = cellRect c
       in Color selectionColor $
            translate x y $
              rectangleSolid w h

-- Conflicts
drawConflicts :: World -> Picture
drawConflicts world
  | not (uiShowConflicts (worldUI world)) = Blank
  | otherwise =
      Pictures
        [ let (x, y, w, h) = cellRect c
           in Color conflictColor $
                translate x y $
                  rectangleSolid w h
        | c <- Set.toList (conflictingCells (gsCurrent (worldGame world)))
        ]

drawErrorCell :: World -> Picture
drawErrorCell world =
  case uiErrorCell (worldUI world) of
    Nothing -> Blank
    Just c  ->
      let (x, y, w, h) = cellRect c
          a = uiErrorAlpha (worldUI world)
       in Color (withAlpha a conflictColor) $
            translate x y $
              rectangleSolid w h

drawSolvedOverlay :: World -> Picture
drawSolvedOverlay world
  | not (uiSolved (worldUI world)) = Blank
  | otherwise =
      Color (withAlpha (uiSolvedAlpha (worldUI world)) solvedHighlightColor) $
        translate (boardLeft + boardSize / 2) (boardTop - boardSize / 2) $
          rectangleSolid boardSize boardSize

-- Status bar
drawStatusBar :: World -> Picture
drawStatusBar world =
  translate (-300) statusY $
    scale 0.15 0.15 $
      Color statusColor $
        Text msg
  where
    msg = fromMaybe "Sudoku" (uiMessage (worldUI world))

-- Geometry helpers
cellCenter :: Cell -> (Float, Float)
cellCenter (Cell i) = (x, y)
  where
    row = i `div` 9
    col = i `mod` 9
    x = boardLeft + fromIntegral col * cellSize + cellSize / 2
    y = boardTop  - fromIntegral row * cellSize - cellSize / 2

cellRect :: Cell -> (Float, Float, Float, Float)
cellRect c = (x, y, cellSize, cellSize)
  where
    (x, y) = cellCenter c

-- Mouse -> Cell
pointToCell :: (Float, Float) -> Maybe Cell
pointToCell (x, y)
  | x < boardLeft || x > boardLeft + boardSize = Nothing
  | y > boardTop  || y < boardTop - boardSize  = Nothing
  | otherwise = Just (Cell (row * 9 + col))
  where
    col = floor ((x - boardLeft) / cellSize)
    row = floor ((boardTop - y) / cellSize)