module Levels
  ( levelFiles
  , builtInPuzzles
  , allBuiltInPuzzles
  , puzzlesForDifficulty
  -- Loading
  , loadPuzzleFromFile
  , parsePuzzleString
  -- Picking puzzle
  , pickRandomPuzzlePath
  , loadRandomPuzzle
  , loadPuzzleByIndex
  ) where

import System.Random (StdGen, randomR)
import qualified Data.Vector as V
import Data.Char (isSpace)

import Types


levelFiles :: [FilePath]
levelFiles =
  [ "levels/Level1.txt"
  , "levels/Level2.txt"
  , "levels/Level3.txt"
  ]

builtInPuzzleStrings :: [(Difficulty, [String])]
builtInPuzzleStrings =
  [ ( Easy
    , [ "530070000600195000098000060800060003400803001700020006060000280000419005000080079"
      , "534008910072195308198040567809761020420803791713024856901537284280419605345286009"
      , "030678902672005340198342007850701423406853090713020856901537204280419635345206170"
      , "504600912672190348108302567859761003426050791703924850061537284287019635345286109"
      , "534678002670105348098340567859061423420853700713904856961500084207419635345280179"
      ]
    )
  , ( Medium
    , [ "500070002070105300198000560800761003420050091003904850061007284207419030300080179"
      , "034000910600195040008040507850060400026803090003020856960500084080419600305086170"
      , "500608010072100308190042007009701020426000701010904800060530084207009630040206009"
      , "030008902602090348108300560859061003020803090700020856061007204287010605300286070"
      , "504600012070005040098342500009760423420050091713024800900507284080419635305080109"
      ]
    )
  , ( Hard
    , [ "000600900070090040000302007009001000020000700003004006000500200007009005005006009"
      , "034000002600090008098000007050060400400050001010020800900030004080010600300080100"
      , "500008900000000340098002500000061003006800790003900850001500280000410035000000179"
      , "030608010002090308008040507050701020006050701010904050060507080207009030040206070"
      , "004670000002100340000040067059000400020053000003000050061000280000400630000280009"
      ]
    )
  ]

builtInPuzzles :: Either String [(Difficulty, [LoadedPuzzle])]
builtInPuzzles =
  traverse loadDifficulty builtInPuzzleStrings
  where
    loadDifficulty (difficulty, raws) =
      case traverse parsePuzzleString raws of
        Left err -> Left err
        Right puzzles -> Right (difficulty, puzzles)

allBuiltInPuzzles :: Either String [LoadedPuzzle]
allBuiltInPuzzles =
  concatMap snd <$> builtInPuzzles

puzzlesForDifficulty :: Difficulty -> Either String [LoadedPuzzle]
puzzlesForDifficulty difficulty =
  case lookup difficulty <$> builtInPuzzles of
    Left err -> Left err
    Right Nothing -> Left "Difficulty not found"
    Right (Just puzzles) -> Right puzzles

-- 81 characters, empty cell — 0, "." or space.
loadPuzzleFromFile :: FilePath -> IO (Either String LoadedPuzzle)
loadPuzzleFromFile path = do
  content <- readFile path
  case lines content of
    []      -> pure (Left "Empty file")
    (p : _) -> pure (parsePuzzleString p)

parsePuzzleString :: String -> Either String LoadedPuzzle
parsePuzzleString input
  | length chars /= 81 = Left "Puzzle must contain exactly 81 cells"
  | otherwise =
      case traverse charToCell chars of
        Left err -> Left err
        Right values ->
          Right $
            LoadedPuzzle
              { lpBoard  = Board (V.fromList values)
              , lpGivens = V.fromList (map (/= 0) values)
              }
  where
    chars = filter (not . isSpace) input

    charToCell :: Char -> Either String Int
    charToCell '.' = Right 0
    charToCell '0' = Right 0
    charToCell c
      | c >= '1' && c <= '9' = Right (read [c])
      | otherwise = Left ("Invalid character: " ++ [c])


pickRandomPuzzlePath :: [FilePath] -> StdGen -> Maybe (FilePath, StdGen)
pickRandomPuzzlePath paths g =
  case paths of
    [] -> Nothing
    _ ->
      let (i, g') = randomR (0, length paths - 1) g
       in Just (paths !! i, g')

loadRandomPuzzle :: StdGen -> IO (Either String LoadedPuzzle)
loadRandomPuzzle g = do
  let paths = levelFiles
  case pickRandomPuzzlePath paths g of
    Nothing ->
      pure (Left "No level files found")
    Just (path, _) ->
      loadPuzzleFromFile path

loadPuzzleByIndex :: Int -> IO (Either String LoadedPuzzle)
loadPuzzleByIndex idx
  | idx < 0 || idx >= length levelFiles =
      pure (Left "Index out of range")
  | otherwise =
      loadPuzzleFromFile (levelFiles !! idx)
