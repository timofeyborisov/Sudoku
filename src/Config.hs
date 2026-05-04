module Config
  ( windowWidth
  , windowHeight
  , windowTitle
  , fps
  , cellSize
  , buttonWidth
  , buttonHeight
  , buttonBorderWidth
  , topBarGap
  , topWideWidth
  , backgroundExtent
  , statusGap
  , thinLineWidth
  , thickLineWidth
  , menuTitleY
  , menuTitleOffsetX
  , menuSubtitleGap
  , menuBackX
  , menuBackY
  , menuThemeX
  , menuThemeY
  , puzzleMenuLeft
  , puzzleMenuTopRowY
  , puzzleMenuBottomRowY
  , puzzleMenuGenerateY
  , puzzleMenuLoadY
  , puzzleMenuOrY
  , puzzleSquareSize
  , puzzleRandomWidth
  , puzzleGenerateWidth
  , backgroundColor
  , thinGridColor
  , thickGridColor
  , selectionColor
  , conflictColor
  , solvedHighlightColor
  , givenDigitColor
  , userDigitColor
  , statusColor
  , darkBackgroundColor
  , darkThinGridColor
  , darkThickGridColor
  , darkSelectionColor
  , darkConflictColor
  , darkSolvedHighlightColor
  , darkGivenDigitColor
  , darkUserDigitColor
  , darkStatusColor
  , darkButtonFillColor
  , darkButtonHoverColor
  , lightButtonHoverColor
  , relatedUnitsAlpha
  , hoverCellAlpha
  ) where

import Graphics.Gloss

-- Window
windowWidth :: Int
windowWidth = 820

windowHeight :: Int
windowHeight = 820

windowTitle :: String
windowTitle = "Sudoku"

fps :: Int
fps = 60

-- Board
cellSize :: Float
cellSize = 66

-- Buttons
buttonWidth :: Float
buttonWidth = 160

buttonHeight :: Float
buttonHeight = 42

buttonBorderWidth :: Float
buttonBorderWidth = 2

topBarGap :: Float
topBarGap = 37

topWideWidth :: Float
topWideWidth = 100

backgroundExtent :: Float
backgroundExtent = 5000

statusGap :: Float
statusGap = 52

thinLineWidth :: Float
thinLineWidth = 1

thickLineWidth :: Float
thickLineWidth = 3

-- Menu layout
menuTitleY :: Float
menuTitleY = 275

menuTitleOffsetX :: Float
menuTitleOffsetX = -10

menuSubtitleGap :: Float
menuSubtitleGap = 45

menuBackX :: Float
menuBackX = 240

menuBackY :: Float
menuBackY = -305

menuThemeX :: Float
menuThemeX = 299

menuThemeY :: Float
menuThemeY = -250

puzzleMenuLeft :: Float
puzzleMenuLeft = -125

puzzleMenuTopRowY :: Float
puzzleMenuTopRowY = 92

puzzleMenuBottomRowY :: Float
puzzleMenuBottomRowY = 26

puzzleMenuGenerateY :: Float
puzzleMenuGenerateY = -94

puzzleMenuLoadY :: Float
puzzleMenuLoadY = 130

puzzleMenuOrY :: Float
puzzleMenuOrY = -58

puzzleSquareSize :: Float
puzzleSquareSize = 52

puzzleRandomWidth :: Float
puzzleRandomWidth = 184

puzzleGenerateWidth :: Float
puzzleGenerateWidth = 250

-- Light theme colors
backgroundColor :: Color
backgroundColor = makeColorI 245 245 245 255

thinGridColor :: Color
thinGridColor = greyN 0.75

thickGridColor :: Color
thickGridColor = greyN 0.25

selectionColor :: Color
selectionColor = makeColorI 228 239 255 255

conflictColor :: Color
conflictColor = makeColorI 255 210 210 255

solvedHighlightColor :: Color
solvedHighlightColor = makeColorI 190 235 190 255

givenDigitColor :: Color
givenDigitColor = greyN 0.15

userDigitColor :: Color
userDigitColor = greyN 0.35

statusColor :: Color
statusColor = greyN 0.2

-- Dark theme colors
darkBackgroundColor :: Color
darkBackgroundColor = makeColorI 27 31 39 255

darkThinGridColor :: Color
darkThinGridColor = makeColorI 82 90 104 255

darkThickGridColor :: Color
darkThickGridColor = makeColorI 196 203 214 255

darkSelectionColor :: Color
darkSelectionColor = makeColorI 58 81 117 255

darkConflictColor :: Color
darkConflictColor = makeColorI 124 63 70 255

darkSolvedHighlightColor :: Color
darkSolvedHighlightColor = makeColorI 70 125 95 255

darkGivenDigitColor :: Color
darkGivenDigitColor = makeColorI 236 239 245 255

darkUserDigitColor :: Color
darkUserDigitColor = makeColorI 184 194 212 255

darkStatusColor :: Color
darkStatusColor = makeColorI 220 226 236 255

darkButtonFillColor :: Color
darkButtonFillColor = makeColorI 74 82 96 255

darkButtonHoverColor :: Color
darkButtonHoverColor = makeColorI 94 103 120 255

lightButtonHoverColor :: Color
lightButtonHoverColor = makeColorI 214 230 255 255

-- Overlay alpha
relatedUnitsAlpha :: Float
relatedUnitsAlpha = 0.28

hoverCellAlpha :: Float
hoverCellAlpha = 0.5
