module Config
  ( windowWidth
  , windowHeight
  , windowTitle
  , fps
  , cellSize
  , backgroundColor
  , thinGridColor
  , thickGridColor
  , selectionColor
  , conflictColor
  , solvedHighlightColor
  , givenDigitColor
  , userDigitColor
  , statusColor
  ) where

import Graphics.Gloss

windowWidth :: Int
windowWidth = 700

windowHeight :: Int
windowHeight = 700

windowTitle :: String
windowTitle = "Sudoku"

fps :: Int
fps = 60

cellSize :: Float
cellSize = 60

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
