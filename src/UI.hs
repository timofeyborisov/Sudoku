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

-- Board geometry
boardSize :: Float
boardSize = 9 * cellSize

boardLeft :: Float
boardLeft = -(boardSize / 2)

boardTop :: Float
boardTop = boardSize / 2

boardRight :: Float
boardRight = boardSize / 2

-- Status bar
statusY :: Float
statusY = -boardTop - statusGap

statusX :: Float
statusX = boardLeft

topBarY :: Float
topBarY = boardTop + topBarGap + buttonHeight / 2

topIconSize :: Float
topIconSize = buttonHeight

menuTitleX :: Float
menuTitleX = boardLeft + menuTitleOffsetX

menuSubtitleY :: Float
menuSubtitleY = menuTitleY - menuSubtitleGap

-- Main render
renderWorld :: World -> Picture
renderWorld world =
  case uiScreen (worldUI world) of
    MainMenu -> drawMainMenu world
    DifficultyMenu -> drawDifficultyMenu world
    PuzzleMenu difficulty -> drawPuzzleMenu world difficulty
    Playing -> drawGameWorld world

drawGameWorld :: World -> Picture
drawGameWorld world =
  Pictures
    [ drawBackground world
    , drawTopBar world
    , drawHoverCell world
    , drawRelatedUnits world
    , drawSelection world
    , drawErrorCell world
    , drawSolvedOverlay world
    , drawConflicts world
    , drawGrid world
    , drawBoard world
    , drawStatusBar world
    ]

-- Background
drawBackground :: World -> Picture
drawBackground world =
  Color (themeBackgroundColor world) $
    rectangleSolid backgroundExtent backgroundExtent

-- Menu screens
drawMainMenu :: World -> Picture
drawMainMenu world =
  Pictures
    [ drawBackground world
    , drawMenuTitle world 0.26 "Sudoku"
    , drawMenuSubtitle world "Choose a puzzle & test your logic!"
    , drawButtonOnScreen world MainMenu ButtonPlay
    , drawButtonOnScreen world MainMenu ButtonTheme
    ]

drawDifficultyMenu :: World -> Picture
drawDifficultyMenu world =
  Pictures
    [ drawBackground world
    , drawMenuTitle world 0.26 "Choose a difficulty"
    , drawButtonOnScreen world DifficultyMenu (ButtonDifficulty Easy)
    , drawButtonOnScreen world DifficultyMenu (ButtonDifficulty Medium)
    , drawButtonOnScreen world DifficultyMenu (ButtonDifficulty Hard)
    , drawButtonOnScreen world DifficultyMenu ButtonBack
    , drawButtonOnScreen world DifficultyMenu ButtonTheme
    ]

drawPuzzleMenu :: World -> Difficulty -> Picture
drawPuzzleMenu world difficulty =
  Pictures
    [ drawBackground world
    , drawMenuTitle world 0.26 (difficultyLabel difficulty ++ " puzzles")
    , drawPuzzleSectionLabel world puzzleMenuLeft puzzleMenuLoadY "LOAD:"
    , drawButtonOnScreen world (PuzzleMenu difficulty) (ButtonPuzzle 1)
    , drawButtonOnScreen world (PuzzleMenu difficulty) (ButtonPuzzle 2)
    , drawButtonOnScreen world (PuzzleMenu difficulty) (ButtonPuzzle 3)
    , drawButtonOnScreen world (PuzzleMenu difficulty) (ButtonPuzzle 4)
    , drawButtonOnScreen world (PuzzleMenu difficulty) (ButtonPuzzle 5)
    , drawButtonOnScreen world (PuzzleMenu difficulty) ButtonRandom
    , drawPuzzleSectionLabel world puzzleMenuLeft puzzleMenuOrY "OR:"
    , drawButtonOnScreen world (PuzzleMenu difficulty) ButtonGenerate
    , drawButtonOnScreen world (PuzzleMenu difficulty) ButtonBack
    , drawButtonOnScreen world (PuzzleMenu difficulty) ButtonTheme
    ]

drawMenuTitle :: World -> Float -> String -> Picture
drawMenuTitle world textScale title =
  translate menuTitleX menuTitleY $
    scale textScale textScale $
      Color (themeThickGridColor world) $
        Text title

drawMenuSubtitle :: World -> String -> Picture
drawMenuSubtitle world subtitle =
  translate menuTitleX menuSubtitleY $
    scale 0.12 0.12 $
      Color (themeStatusColor world) $
        Text subtitle

drawPuzzleSectionLabel :: World -> Float -> Float -> String -> Picture
drawPuzzleSectionLabel world x y label =
  translate x y $
    scale 0.18 0.18 $
      Color (themeThickGridColor world) $
        Text label

drawTopBar :: World -> Picture
drawTopBar world =
  Pictures
    [ drawButtonOnScreen world Playing ButtonHint
    , drawButtonOnScreen world Playing ButtonSolve
    , drawButtonOnScreen world Playing ButtonTheme
    , drawButtonOnScreen world Playing ButtonReset
    , drawButtonOnScreen world Playing ButtonMenu
    ]

-- Buttons
drawButtonOnScreen :: World -> Screen -> ButtonAction -> Picture
drawButtonOnScreen world screen action =
  case buttonRectForScreen screen action of
    Nothing -> Blank
    Just rect ->
      drawMenuButton world hovered rect action
  where
    hovered = uiHoverButton (worldUI world) == Just action

drawMenuButton :: World -> Bool -> (Float, Float, Float, Float) -> ButtonAction -> Picture
drawMenuButton world hovered (x, y, w, h) action =
  Pictures
    ( [ Color fillColor $
        translate x y $
          rectangleSolid w h
      , Color (themeThickGridColor world) $
        translate x (y + h / 2) $
          rectangleSolid w buttonBorderWidth
      , Color (themeThickGridColor world) $
        translate x (y - h / 2) $
          rectangleSolid w buttonBorderWidth
      , Color (themeThickGridColor world) $
        translate (x - w / 2) y $
          rectangleSolid buttonBorderWidth h
      , Color (themeThickGridColor world) $
        translate (x + w / 2) y $
          rectangleSolid buttonBorderWidth h
      ]
    ++ textPicture
    ++ iconPicture
      )
  where
    label = buttonLabel world action
    fillColor
      | hovered = themeButtonHoverColor world
      | otherwise = themeButtonFillColor world
    
    textPicture =
      case label of
        "" -> []
        _ ->
          [ translate (textX label action x w) (y - 8) $
              scale 0.14 0.14 $
                Color (themeStatusColor world) $
                  Text label
          ]

    iconPicture =
      case buttonIcon world action of
        Nothing -> []
        Just icon ->
          [ drawIconForButton action (x, y, w, h) icon ]

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
buttonsForScreen MainMenu = [ButtonPlay, ButtonTheme]
buttonsForScreen DifficultyMenu =
  [ ButtonDifficulty Easy
  , ButtonDifficulty Medium
  , ButtonDifficulty Hard
  , ButtonBack
  , ButtonTheme
  ]
buttonsForScreen (PuzzleMenu _) =
  [ ButtonPuzzle 1
  , ButtonPuzzle 2
  , ButtonPuzzle 3
  , ButtonPuzzle 4
  , ButtonPuzzle 5
  , ButtonRandom
  , ButtonGenerate
  , ButtonBack
  , ButtonTheme
  ]
buttonsForScreen Playing = [ButtonHint, ButtonSolve, ButtonTheme, ButtonReset, ButtonMenu]

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
buttonRectForScreen MainMenu ButtonPlay = centerButton 0
buttonRectForScreen MainMenu ButtonTheme = menuIconButton
buttonRectForScreen DifficultyMenu (ButtonDifficulty Easy) = centerButton 75
buttonRectForScreen DifficultyMenu (ButtonDifficulty Medium) = centerButton 0
buttonRectForScreen DifficultyMenu (ButtonDifficulty Hard) = centerButton (-75)
buttonRectForScreen DifficultyMenu ButtonBack = backButton
buttonRectForScreen DifficultyMenu ButtonTheme = menuIconButton
buttonRectForScreen (PuzzleMenu _) (ButtonPuzzle 1) = puzzleSquareButton (-99) puzzleMenuTopRowY
buttonRectForScreen (PuzzleMenu _) (ButtonPuzzle 2) = puzzleSquareButton (-33) puzzleMenuTopRowY
buttonRectForScreen (PuzzleMenu _) (ButtonPuzzle 3) = puzzleSquareButton 33 puzzleMenuTopRowY
buttonRectForScreen (PuzzleMenu _) (ButtonPuzzle 4) = puzzleSquareButton 99 puzzleMenuTopRowY
buttonRectForScreen (PuzzleMenu _) (ButtonPuzzle 5) = puzzleSquareButton (-99) puzzleMenuBottomRowY
buttonRectForScreen (PuzzleMenu _) ButtonRandom =
  Just (33, puzzleMenuBottomRowY, puzzleRandomWidth, puzzleSquareSize)
buttonRectForScreen (PuzzleMenu _) ButtonGenerate =
  Just (0, puzzleMenuGenerateY, puzzleGenerateWidth, buttonHeight)
buttonRectForScreen (PuzzleMenu _) ButtonBack = backButton
buttonRectForScreen (PuzzleMenu _) ButtonTheme = menuIconButton
buttonRectForScreen Playing ButtonHint = Just (boardLeft + topIconSize / 2, topBarY, topIconSize, topIconSize)
buttonRectForScreen Playing ButtonSolve = Just (boardLeft + topIconSize + topBarGap + topWideWidth / 2, topBarY, topWideWidth, buttonHeight)
buttonRectForScreen Playing ButtonTheme = Just (60, topBarY, topIconSize, topIconSize)
buttonRectForScreen Playing ButtonReset = Just (139, topBarY, topIconSize, topIconSize)
buttonRectForScreen Playing ButtonMenu = Just (boardRight - topWideWidth / 2, topBarY, topWideWidth, buttonHeight)
buttonRectForScreen _ _ = Nothing

centerButton :: Float -> Maybe (Float, Float, Float, Float)
centerButton y = Just (0, y, buttonWidth, buttonHeight)

backButton :: Maybe (Float, Float, Float, Float)
backButton = Just (menuBackX, menuBackY, buttonWidth, buttonHeight)

menuIconButton :: Maybe (Float, Float, Float, Float)
menuIconButton =
  Just
    ( menuThemeX
    , menuThemeY
    , topIconSize
    , topIconSize
    )

puzzleSquareButton :: Float -> Float -> Maybe (Float, Float, Float, Float)
puzzleSquareButton x y = Just (x, y, puzzleSquareSize, puzzleSquareSize)

buttonLabel :: World -> ButtonAction -> String
buttonLabel _ ButtonPlay = "Play"
buttonLabel _ (ButtonDifficulty difficulty) = difficultyLabel difficulty
buttonLabel _ (ButtonPuzzle idx) = show idx
buttonLabel _ ButtonRandom = "Random level"
buttonLabel _ ButtonGenerate = "Generate"
buttonLabel _ ButtonBack = "Back"
buttonLabel _ ButtonSolve = "Solve"
buttonLabel _ ButtonHint = ""
buttonLabel _ ButtonTheme = ""
buttonLabel _ ButtonReset = ""
buttonLabel _ ButtonMenu = "Menu"

difficultyLabel :: Difficulty -> String
difficultyLabel Easy = "Easy"
difficultyLabel Medium = "Medium"
difficultyLabel Hard = "Hard"

textOffset :: String -> Float
textOffset "Random level" = 51
textOffset "Generate" = 32
textOffset label = fromIntegral (length label) * 4.5

textX :: String -> ButtonAction -> Float -> Float -> Float
textX _ ButtonRandom x _ = x - 74
textX _ ButtonGenerate x _ = x - 38
textX label _ x _ = x - textOffset label

buttonIcon :: World -> ButtonAction -> Maybe Picture
buttonIcon world action =
  case action of
    ButtonHint -> Just (themeAsset world uiBulbIcon uiBulbIconWhite)
    ButtonTheme -> Just (themeAsset world uiThemeIcon uiThemeIconWhite)
    ButtonReset -> Just (themeAsset world uiResetIcon uiResetIconWhite)
    ButtonRandom -> Just (themeAsset world uiDiceIcon uiDiceIconWhite)
    ButtonGenerate -> Just (themeAsset world uiGenerateIcon uiGenerateIconWhite)
    _ -> Nothing

themeAsset :: World -> (UIAssets -> Picture) -> (UIAssets -> Picture) -> Picture
themeAsset world darkAsset lightAsset =
  case uiTheme (worldUI world) of
    LightTheme -> darkAsset (worldAssets world)
    DarkTheme -> lightAsset (worldAssets world)

drawIconForButton :: ButtonAction -> (Float, Float, Float, Float) -> Picture -> Picture
drawIconForButton action (x, y, w, _) icon =
  case action of
    ButtonHint ->
      translate x y $
        scale 0.048 0.048 icon
    ButtonTheme ->
      translate x y $
        scale 0.05 0.05 icon
    ButtonReset ->
      translate x y $
        scale 0.05 0.05 icon
    ButtonRandom ->
      translate (x + w / 2 - 24) y $
        scale 0.04 0.04 icon
    ButtonGenerate ->
      translate (x + w / 2 - 24) y $
        scale 0.04 0.04 icon
    _ -> Blank

themeBackgroundColor :: World -> Color
themeBackgroundColor world =
  case uiTheme (worldUI world) of
    LightTheme -> backgroundColor
    DarkTheme -> darkBackgroundColor

themeThinGridColor :: World -> Color
themeThinGridColor world =
  case uiTheme (worldUI world) of
    LightTheme -> thinGridColor
    DarkTheme -> darkThinGridColor

themeThickGridColor :: World -> Color
themeThickGridColor world =
  case uiTheme (worldUI world) of
    LightTheme -> thickGridColor
    DarkTheme -> darkThickGridColor

themeSelectionColor :: World -> Color
themeSelectionColor world =
  case uiTheme (worldUI world) of
    LightTheme -> selectionColor
    DarkTheme -> darkSelectionColor

themeConflictColor :: World -> Color
themeConflictColor world =
  case uiTheme (worldUI world) of
    LightTheme -> conflictColor
    DarkTheme -> darkConflictColor

themeSolvedColor :: World -> Color
themeSolvedColor world =
  case uiTheme (worldUI world) of
    LightTheme -> solvedHighlightColor
    DarkTheme -> darkSolvedHighlightColor

themeGivenDigitColor :: World -> Color
themeGivenDigitColor world =
  case uiTheme (worldUI world) of
    LightTheme -> givenDigitColor
    DarkTheme -> darkGivenDigitColor

themeUserDigitColor :: World -> Color
themeUserDigitColor world =
  case uiTheme (worldUI world) of
    LightTheme -> userDigitColor
    DarkTheme -> darkUserDigitColor

themeStatusColor :: World -> Color
themeStatusColor world =
  case uiTheme (worldUI world) of
    LightTheme -> statusColor
    DarkTheme -> darkStatusColor

themeButtonFillColor :: World -> Color
themeButtonFillColor world =
  case uiTheme (worldUI world) of
    LightTheme -> selectionColor
    DarkTheme -> darkButtonFillColor

themeButtonHoverColor :: World -> Color
themeButtonHoverColor world =
  case uiTheme (worldUI world) of
    LightTheme -> lightButtonHoverColor
    DarkTheme -> darkButtonHoverColor

relatedUnitsColor :: World -> Color
relatedUnitsColor world =
  withAlpha relatedUnitsAlpha (themeSelectionColor world)

hoverCellColor :: World -> Color
hoverCellColor world =
  withAlpha hoverCellAlpha (themeSelectionColor world)

-- Grid
thinLineIndices :: [Int]
thinLineIndices = [1, 2, 4, 5, 7, 8]

thickInnerLineIndices :: [Int]
thickInnerLineIndices = [3, 6]

xAt :: Int -> Float
xAt i = boardLeft + fromIntegral i * cellSize

yAt :: Int -> Float
yAt i = boardTop - fromIntegral i * cellSize

drawGrid :: World -> Picture
drawGrid world =
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
      [ Color (themeThinGridColor world) $
          translate (xAt i) centerY $
            rectangleSolid thinLineWidth boardSize
      | i <- thinLineIndices
      ]

    thinHorizontal =
      [ Color (themeThinGridColor world) $
          translate centerX (yAt i) $
            rectangleSolid boardSize thinLineWidth
      | i <- thinLineIndices
      ]

    thickVerticalInner =
      [ Color (themeThickGridColor world) $
          translate (xAt i) centerY $
            rectangleSolid thickLineWidth (boardSize - thickLineWidth)
      | i <- thickInnerLineIndices
      ]

    thickHorizontalInner =
      [ Color (themeThickGridColor world) $
          translate centerX (yAt i) $
            rectangleSolid (boardSize - thickLineWidth) thickLineWidth
      | i <- thickInnerLineIndices
      ]

    outerFrame =
      [ Color (themeThickGridColor world) $
          translate centerX (yAt 0) $
            rectangleSolid (boardSize + thickLineWidth) thickLineWidth

      , Color (themeThickGridColor world) $
          translate centerX (yAt 9) $
            rectangleSolid (boardSize + thickLineWidth) thickLineWidth

      , Color (themeThickGridColor world) $
          translate (xAt 0) centerY $
            rectangleSolid thickLineWidth boardSize

      , Color (themeThickGridColor world) $
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
      if isGiven gs c
        then themeGivenDigitColor world
        else themeUserDigitColor world

isGiven :: GameState -> Cell -> Bool
isGiven gs (Cell i) = gsGivens gs V.! i

-- Selection
drawRelatedUnits :: World -> Picture
drawRelatedUnits world =
  case uiSelected (worldUI world) of
    Nothing -> Blank
    Just cell ->
      Pictures
        [ let (x, y, w, h) = cellRect c
           in Color (relatedUnitsColor world) $
                translate x y $
                  rectangleSolid w h
        | c <- relatedUnitCells cell
        ]

relatedUnitCells :: Cell -> [Cell]
relatedUnitCells cell =
  filter (/= cell) $
    Set.toList $
      Set.fromList
        ( rowIndices (rowOfCell cell) ++
          colIndices (colOfCell cell) ++
          boxIndices (boxOfCell cell)
        )

drawHoverCell :: World -> Picture
drawHoverCell world =
  case uiHoverCell (worldUI world) of
    Nothing -> Blank
    Just cell
      | uiSelected (worldUI world) == Just cell -> Blank
      | otherwise ->
          let (x, y, w, h) = cellRect cell
           in Color (hoverCellColor world) $
                translate x y $
                  rectangleSolid w h

drawSelection :: World -> Picture
drawSelection world =
  case uiSelected (worldUI world) of
    Nothing -> Blank
    Just c  ->
      let (x, y, w, h) = cellRect c
       in Color (themeSelectionColor world) $
            translate x y $
              rectangleSolid w h

-- Conflict highlight
drawConflicts :: World -> Picture
drawConflicts world
  | not (uiShowConflicts (worldUI world)) = Blank
  | otherwise =
      Pictures
        [ let (x, y, w, h) = cellRect c
           in Color (themeConflictColor world) $
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
       in Color (withAlpha a (themeConflictColor world)) $
            translate x y $
              rectangleSolid w h

drawSolvedOverlay :: World -> Picture
drawSolvedOverlay world
  | not (uiSolved (worldUI world)) = Blank
  | otherwise =
      Color (withAlpha (uiSolvedAlpha (worldUI world)) (themeSolvedColor world)) $
        translate (boardLeft + boardSize / 2) (boardTop - boardSize / 2) $
          rectangleSolid boardSize boardSize

-- Status bar
drawStatusBar :: World -> Picture
drawStatusBar world =
  translate statusX statusY $
    scale 0.15 0.15 $
      Color (themeStatusColor world) $
        Text msg
  where
    msg =
      fromMaybe "Sudoku" (uiMessage (worldUI world)) ++
      "    Moves: " ++ show (moveCount world)

moveCount :: World -> Int
moveCount world =
  length
    [ cell
    | cell <- allCells
    , not (isGiven gs cell)
    , boardGet (gsCurrent gs) cell /= 0
    ]
  where
    gs = worldGame world

-- Cell geometry
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

-- Mouse input
pointToCell :: (Float, Float) -> Maybe Cell
pointToCell (x, y)
  | x < boardLeft || x >= boardLeft + boardSize = Nothing
  | y > boardTop  || y <= boardTop - boardSize  = Nothing
  | otherwise = Just (Cell (row * 9 + col))
  where
    col = floor ((x - boardLeft) / cellSize)
    row = floor ((boardTop - y) / cellSize)
