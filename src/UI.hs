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
  , buttonAt
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

statusX :: Float
statusX = -270

thinLineWidth :: Float
thinLineWidth = 1

thickLineWidth :: Float
thickLineWidth = 3

buttonWidth :: Float
buttonWidth = 160

buttonHeight :: Float
buttonHeight = 42

buttonBorderWidth :: Float
buttonBorderWidth = 2

menuTitleX :: Float
menuTitleX = boardLeft - 10

menuTitleY :: Float
menuTitleY = 250

menuSubtitleY :: Float
menuSubtitleY = menuTitleY - 45

-- Main render
renderWorld :: World -> Picture
renderWorld world =
  case uiScreen (worldUI world) of
    MainMenu -> drawMainMenu
    DifficultyMenu -> drawDifficultyMenu
    PuzzleMenu difficulty -> drawPuzzleMenu difficulty
    Playing -> drawGameWorld world

drawGameWorld :: World -> Picture
drawGameWorld world =
  Pictures
    [ drawBackground
    , drawTopBar
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

drawMainMenu :: Picture
drawMainMenu =
  Pictures
    [ drawBackground
    , drawMenuTitle 0.26 "Sudoku"
    , drawMenuSubtitle "Choose a puzzle and test your logic."
    , drawButtonOnScreen MainMenu ButtonPlay
    ]

drawDifficultyMenu :: Picture
drawDifficultyMenu =
  Pictures
    [ drawBackground
    , drawMenuTitle 0.26 "Choose a difficulty"
    , drawButtonOnScreen DifficultyMenu (ButtonDifficulty Easy)
    , drawButtonOnScreen DifficultyMenu (ButtonDifficulty Medium)
    , drawButtonOnScreen DifficultyMenu (ButtonDifficulty Hard)
    , drawButtonOnScreen DifficultyMenu ButtonBack
    ]

drawPuzzleMenu :: Difficulty -> Picture
drawPuzzleMenu difficulty =
  Pictures
    [ drawBackground
    , drawMenuTitle 0.26 (difficultyLabel difficulty ++ " puzzles")
    , drawButtonOnScreen (PuzzleMenu difficulty) (ButtonPuzzle 1)
    , drawButtonOnScreen (PuzzleMenu difficulty) (ButtonPuzzle 2)
    , drawButtonOnScreen (PuzzleMenu difficulty) (ButtonPuzzle 3)
    , drawButtonOnScreen (PuzzleMenu difficulty) (ButtonPuzzle 4)
    , drawButtonOnScreen (PuzzleMenu difficulty) (ButtonPuzzle 5)
    , drawButtonOnScreen (PuzzleMenu difficulty) ButtonRandom
    , drawButtonOnScreen (PuzzleMenu difficulty) ButtonBack
    ]

drawMenuTitle :: Float -> String -> Picture
drawMenuTitle textScale title =
  translate menuTitleX menuTitleY $
    scale textScale textScale $
      Color thickGridColor $
        Text title

drawMenuSubtitle :: String -> Picture
drawMenuSubtitle subtitle =
  translate menuTitleX menuSubtitleY $
    scale 0.12 0.12 $
      Color statusColor $
        Text subtitle

drawTopBar :: Picture
drawTopBar =
  Pictures
    [ drawButtonOnScreen Playing ButtonHint
    , drawButtonOnScreen Playing ButtonSolve
    , drawButtonOnScreen Playing ButtonMenu
    ]

drawButtonOnScreen :: Screen -> ButtonAction -> Picture
drawButtonOnScreen screen action =
  case buttonRectForScreen screen action of
    Nothing -> Blank
    Just rect ->
      drawMenuButton rect (buttonLabel action)

drawMenuButton :: (Float, Float, Float, Float) -> String -> Picture
drawMenuButton (x, y, w, h) label =
  Pictures
    [ Color buttonFillColor $
        translate x y $
          rectangleSolid w h
    , Color thickGridColor $
        translate x (y + h / 2) $
          rectangleSolid w buttonBorderWidth
    , Color thickGridColor $
        translate x (y - h / 2) $
          rectangleSolid w buttonBorderWidth
    , Color thickGridColor $
        translate (x - w / 2) y $
          rectangleSolid buttonBorderWidth h
    , Color thickGridColor $
        translate (x + w / 2) y $
          rectangleSolid buttonBorderWidth h
    , translate (x - textOffset label) (y - 8) $
        scale 0.14 0.14 $
          Color statusColor $
            Text label
    ]

buttonAt :: World -> (Float, Float) -> Maybe ButtonAction
buttonAt world point =
  let screen = uiScreen (worldUI world)
   in
  case
    [ action
    | action <- buttonsForScreen screen
    , pointInButton screen point action
    ] of
      [] -> Nothing
      action : _ -> Just action

buttonsForScreen :: Screen -> [ButtonAction]
buttonsForScreen MainMenu = [ButtonPlay]
buttonsForScreen DifficultyMenu =
  [ ButtonDifficulty Easy
  , ButtonDifficulty Medium
  , ButtonDifficulty Hard
  , ButtonBack
  ]
buttonsForScreen (PuzzleMenu _) =
  [ ButtonPuzzle 1
  , ButtonPuzzle 2
  , ButtonPuzzle 3
  , ButtonPuzzle 4
  , ButtonPuzzle 5
  , ButtonRandom
  , ButtonBack
  ]
buttonsForScreen Playing = [ButtonHint, ButtonSolve, ButtonMenu]

pointInButton :: Screen -> (Float, Float) -> ButtonAction -> Bool
pointInButton screen (px, py) action =
  case buttonRectForScreen screen action of
    Nothing -> False
    Just (x, y, w, h) ->
      px >= x - w / 2 &&
      px <= x + w / 2 &&
      py >= y - h / 2 &&
      py <= y + h / 2

buttonRectForScreen :: Screen -> ButtonAction -> Maybe (Float, Float, Float, Float)
buttonRectForScreen MainMenu ButtonPlay = centerButton (-15)
buttonRectForScreen DifficultyMenu (ButtonDifficulty Easy) = centerButton 75
buttonRectForScreen DifficultyMenu (ButtonDifficulty Medium) = centerButton 0
buttonRectForScreen DifficultyMenu (ButtonDifficulty Hard) = centerButton (-75)
buttonRectForScreen DifficultyMenu ButtonBack = backButton
buttonRectForScreen (PuzzleMenu _) (ButtonPuzzle 1) = centerButton 110
buttonRectForScreen (PuzzleMenu _) (ButtonPuzzle 2) = centerButton 55
buttonRectForScreen (PuzzleMenu _) (ButtonPuzzle 3) = centerButton 0
buttonRectForScreen (PuzzleMenu _) (ButtonPuzzle 4) = centerButton (-55)
buttonRectForScreen (PuzzleMenu _) (ButtonPuzzle 5) = centerButton (-110)
buttonRectForScreen (PuzzleMenu _) ButtonRandom = centerButton (-180)
buttonRectForScreen (PuzzleMenu _) ButtonBack = backButton
buttonRectForScreen Playing ButtonHint = topBarButton (-190)
buttonRectForScreen Playing ButtonSolve = topBarButton 0
buttonRectForScreen Playing ButtonMenu = topBarButton 190
buttonRectForScreen _ _ = Nothing

centerButton :: Float -> Maybe (Float, Float, Float, Float)
centerButton y = Just (0, y, buttonWidth, buttonHeight)

backButton :: Maybe (Float, Float, Float, Float)
backButton = Just (240, -250, buttonWidth, buttonHeight)

topBarButton :: Float -> Maybe (Float, Float, Float, Float)
topBarButton x = Just (x, 325, buttonWidth, buttonHeight)

buttonLabel :: ButtonAction -> String
buttonLabel ButtonPlay = "Play"
buttonLabel (ButtonDifficulty difficulty) = difficultyLabel difficulty
buttonLabel (ButtonPuzzle idx) = show idx
buttonLabel ButtonRandom = "Random"
buttonLabel ButtonBack = "Back"
buttonLabel ButtonHint = "Hint"
buttonLabel ButtonSolve = "Solve"
buttonLabel ButtonMenu = "Menu"

difficultyLabel :: Difficulty -> String
difficultyLabel Easy = "Easy"
difficultyLabel Medium = "Medium"
difficultyLabel Hard = "Hard"

buttonFillColor :: Color
buttonFillColor = selectionColor

textOffset :: String -> Float
textOffset label = fromIntegral (length label) * 4.5

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
  translate statusX statusY $
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
  | x < boardLeft || x >= boardLeft + boardSize = Nothing
  | y > boardTop  || y <= boardTop - boardSize  = Nothing
  | otherwise = Just (Cell (row * 9 + col))
  where
    col = floor ((x - boardLeft) / cellSize)
    row = floor ((boardTop - y) / cellSize)
