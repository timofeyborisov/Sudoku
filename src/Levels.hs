module Levels
  ( builtInPuzzles
  ) where

import qualified Data.Vector as V
import Data.Char (isSpace)

import Types

-- Built-in puzzles
builtInPuzzleStrings :: [(Difficulty, [String])]
builtInPuzzleStrings =
  [ ( Easy
    , [ "034608012070195000198040007059701403020000700700924850961037284087419005000286070"
      , "504070902070105348100302000859700023406850091713020856060030080200400035345280170"
      , "534608902670090000198002560800760003020803791703004806900507284287400600000280179"
      , "004008912672100348000042567009061420400003791013004056900007084007400605345206079"
      , "030608012672005308098000560050760023000053791703924056961000204280409605045200009"
      ]
    )
  , ( Medium
    , [ "034608012070195000198040007009701003020000000700920050901037280087419005000206070"
      , "534600012070000040090040060809001420020803001713024800000037204007409600040200179"
      , "000600910002095340090342007000760023000003700003000006001507284007400035345286079"
      , "004078000602095340090000067050000403420050090000924850901007284280400605340000109"
      , "030078912600000348008300060000060400406803000013924856000537284280000600040006070"
      ]
    )
  , ( Hard
    , [ "000078900600095040100000560009761000400800701013004850000000204000419605000080000"
      , "034608012070095000108040000000701003020000000000920050901037280080419000000006070"
      , "504070902070005300100300000809700003406850091003000056060000080200000030305280000"
      , "534608002600090000198000560800060000020800091000004806900507284200000000000000109"
      , "000000900000000040000302507000060003026850001713904006960507204000009000040086079"
      ]
    )
  ]

-- Puzzle parsing
builtInPuzzles :: Either String [(Difficulty, [LoadedPuzzle])]
builtInPuzzles =
  traverse loadDifficulty builtInPuzzleStrings

loadDifficulty :: (Difficulty, [String]) -> Either String (Difficulty, [LoadedPuzzle])
loadDifficulty (difficulty, raws) =
  case traverse parsePuzzleString raws of
    Left err -> Left err
    Right puzzles -> Right (difficulty, puzzles)

parsePuzzleString :: String -> Either String LoadedPuzzle
parsePuzzleString input
  | length chars /= 81 = Left "Puzzle must contain exactly 81 cells"
  | otherwise =
      case traverse puzzleCharToCell chars of
        Left err -> Left err
        Right values ->
          Right $
            LoadedPuzzle
              { lpBoard  = Board (V.fromList values)
              , lpGivens = V.fromList (map (/= 0) values)
              }
  where
    chars = filter (not . isSpace) input

puzzleCharToCell :: Char -> Either String Int
puzzleCharToCell '.' = Right 0
puzzleCharToCell '0' = Right 0
puzzleCharToCell c
  | c >= '1' && c <= '9' = Right (read [c])
  | otherwise = Left ("Invalid character: " ++ [c])
