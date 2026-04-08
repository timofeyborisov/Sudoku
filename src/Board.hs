module Board
  ( -- Indexing
    cellToRC
  , rcToCell
  , rowOfCell
  , colOfCell
  , boxOfCell
  , rowIndices
  , colIndices
  , boxIndices
  , unitIndices
  , allCells

  -- Board
  , emptyBoard
  , boardGet
  , boardSet
  , boardClear

  -- Rules
  , candidatesAt
  , checkMove
  , hasConflictAt
  , conflictingCells
  , isUnitCompleteAndValid
  , isSolved

  -- Game
  , applyMove
  , gameFromLoaded
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Vector (Vector)
import qualified Data.Vector as V

import Types


-- Helpers

digits :: [Digit]
digits = [Digit d | d <- [1 .. 9]]

validUnitIndex :: Int -> Bool
validUnitIndex i = i >= 0 && i < 9

isValidCell :: Cell -> Bool
isValidCell (Cell i) = i >= 0 && i < 81

isGivenCell :: Vector Bool -> Cell -> Bool
isGivenCell givens (Cell i) =
  i >= 0 && i < V.length givens && givens V.! i

unitsOfCell :: Cell -> [LineKind]
unitsOfCell c =
  [ Row (rowOfCell c)
  , Col (colOfCell c)
  , Box (boxOfCell c)
  ]

duplicateValues :: [Int] -> Set Int
duplicateValues xs =
  Set.fromList
    [ x
    | x <- xs
    , x /= 0
    , length (filter (== x) xs) > 1
    ]

conflictsInUnit :: Board -> LineKind -> Set Cell
conflictsInUnit board lk =
  Set.fromList
    [ c
    | c <- unitIndices lk
    , boardGet board c `Set.member` dups
    ]
  where
    dups = duplicateValues [boardGet board c | c <- unitIndices lk]

allUnits :: [LineKind]
allUnits =
  [ Row i | i <- [0 .. 8] ] ++
  [ Col i | i <- [0 .. 8] ] ++
  [ Box i | i <- [0 .. 8] ]


-- Converters (RC - row and column of the cell)
cellToRC :: Cell -> (Int, Int)
cellToRC (Cell i) = (i `div` 9, i `mod` 9)

rcToCell :: Int -> Int -> Cell
rcToCell r c = Cell (r * 9 + c)

rowOfCell :: Cell -> Int
rowOfCell = fst . cellToRC

colOfCell :: Cell -> Int
colOfCell = snd . cellToRC

boxOfCell :: Cell -> Int
boxOfCell cell = 
  let r = rowOfCell cell
      c = colOfCell cell
   in (r `div` 3) * 3 + (c `div` 3)


-- All indicies of the defined lineKind
rowIndices :: Int -> [Cell]
rowIndices r 
  | not (validUnitIndex r) = []
  | otherwise = [rcToCell r c | c <- [0 .. 8]]

colIndices :: Int -> [Cell]
colIndices c 
  | not (validUnitIndex c) = []
  | otherwise = [rcToCell r c | r <- [0 .. 8]]

boxIndices :: Int -> [Cell]
boxIndices b 
  | not (validUnitIndex b) = []
  | otherwise =
      [ rcToCell (baseRow + dr) (baseCol + dc)
      | dr <- [0 .. 2]
      , dc <- [0 .. 2]
      ]
  where
    baseRow = (b `div` 3) * 3
    baseCol = (b `mod` 3) * 3

unitIndices :: LineKind -> [Cell]
unitIndices (Row r) = rowIndices r
unitIndices (Col c) = colIndices c
unitIndices (Box b) = boxIndices b

allCells :: [Cell]
allCells = [Cell i | i <- [0 .. 80]]


-- Board processing
-- Define 9x9 board
emptyBoard :: Board
emptyBoard = Board (V.replicate 81 0)

-- Get cell value
boardGet :: Board -> Cell -> Int
boardGet (Board v) (Cell i) = v V.! i

-- Preliminary check is assumed!
boardSet :: Board -> Cell -> Digit -> Board
boardSet (Board v) (Cell i) (Digit d) = Board (v V.// [(i, d)])

boardClear :: Board -> Cell -> Board
boardClear (Board v) (Cell i) = Board (v V.// [(i, 0)])


-- Conflicts 
-- Set of candidates (what can be put in the cell)
candidatesAt :: Board -> Cell -> Set Digit
candidatesAt board cell
  | not (isValidCell cell) = Set.empty
  | boardGet board cell /= 0 = Set.empty
  | otherwise =
      Set.fromList
        [ d
        | d <- digits
        , not (hasConflictAt board cell d)
        ]

checkMove :: Board -> Vector Bool -> Cell -> Digit -> MoveValidity
checkMove board givens cell digit
  | not (isValidCell cell) = MoveInvalidCell
  | isGivenCell givens cell = MoveImmutable
  | hasConflictAt board cell digit = MoveConflict
  | otherwise = MoveOk

hasConflictAt :: Board -> Cell -> Digit -> Bool
hasConflictAt board cell (Digit d)
  | not (isValidCell cell) = False
  | otherwise =
      any unitHasConflict (unitsOfCell cell)
  where
    unitHasConflict lk =
      any (\c -> c /= cell && boardGet board c == d) (unitIndices lk)

conflictingCells :: Board -> Set Cell
conflictingCells board =
  Set.unions [conflictsInUnit board lk | lk <- allUnits]

isUnitCompleteAndValid :: Board -> LineKind -> Bool
isUnitCompleteAndValid board lk =
  let vals = [boardGet board c | c <- unitIndices lk]
   in length vals == 9
      && notElem 0 vals
      && Set.size (Set.fromList vals) == 9

isSolved :: Board -> Bool
isSolved board =
  all (isUnitCompleteAndValid board) allUnits


-- Application
applyMove :: GameState -> Cell -> Digit -> Either MoveValidity GameState
applyMove gs cell digit =
  case checkMove (gsCurrent gs) (gsGivens gs) cell digit of
    MoveOk ->
      Right gs { gsCurrent = boardSet (gsCurrent gs) cell digit }
    err ->
      Left err

gameFromLoaded :: LoadedPuzzle -> GameState
gameFromLoaded lp =
  GameState
    { gsInitial = lpBoard lp
    , gsCurrent = lpBoard lp
    , gsGivens  = lpGivens lp
    }
