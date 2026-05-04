module Events
  ( handleEvent
  , updateWorld
  ) where

import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Vector as V
import System.Random (StdGen, mkStdGen, randomR)

import Types
import Board
import Generator
import Solver
import UI

-- Timing
solveStepDelay :: Float
solveStepDelay = 0.04

statusDelay :: Float
statusDelay = 0.85

-- Main dispatcher
handleEvent :: Event -> World -> World
handleEvent event world =
  case event of
    EventKey (MouseButton LeftButton) Down _ pos ->
      handleClick pos world

    EventMotion pos ->
      updateHover pos world

    EventKey (Char ch) Down _ _ ->
      handleCharacter ch world

    EventKey key Down _ _ ->
      handleSpecialKey key world

    _ ->
      world

-- Mouse input
handleClick :: (Float, Float) -> World -> World
handleClick pos world =
  case buttonAt world pos of
    Just action ->
      handleButton action world

    Nothing ->
      case uiScreen (worldUI world) of
        Playing
          | not (uiSolved (worldUI world)) && not (isSolving world) ->
              selectBoardCell pos world

        _ ->
          world

-- Button actions
handleButton :: ButtonAction -> World -> World
handleButton ButtonPlay world =
  world
    { worldUI =
        (worldUI world)
          { uiScreen = DifficultyMenu
          , uiMessage = Just "Sudoku"
          }
    }
handleButton (ButtonDifficulty difficulty) world =
  world
    { worldUI =
        resetTransientUI (worldUI world)
          { uiScreen = PuzzleMenu difficulty
          }
    }
handleButton (ButtonPuzzle idx) world =
  case uiScreen (worldUI world) of
    PuzzleMenu difficulty -> startLevel difficulty (idx - 1) world
    _ -> world
handleButton ButtonRandom world =
  case pickRandomPuzzle world of
    Nothing ->
      world
        { worldUI =
            (worldUI world)
              { uiMessage = Just "No puzzles available"
              }
        }

    Just (loaded, seed') ->
      startLoadedPuzzle loaded world
        { worldRandomSeed = seed'
        }
handleButton ButtonGenerate world =
  case uiScreen (worldUI world) of
    PuzzleMenu difficulty ->
      let gen = worldGenerator world
          (loaded, gen') = generatePuzzle difficulty gen
          seed' = nextGeneratorSeed gen'
       in startLoadedPuzzle loaded world
            { worldRandomSeed = seed'
            }
    _ -> world
handleButton ButtonBack world =
  case uiScreen (worldUI world) of
    DifficultyMenu ->
      world
        { worldUI =
            resetTransientUI (worldUI world)
              { uiScreen = MainMenu
              }
        }

    PuzzleMenu _ ->
      world
        { worldUI =
            resetTransientUI (worldUI world)
              { uiScreen = DifficultyMenu
              }
        }

    _ ->
      world
handleButton ButtonHint world = showHint world
handleButton ButtonSolve world = startSolverAnimation world
handleButton ButtonTheme world =
  world
    { worldUI =
        (worldUI world)
          { uiTheme = toggleTheme (uiTheme (worldUI world))
          }
    }
handleButton ButtonReset world = resetCurrentLevel world
handleButton ButtonMenu world =
  world
    { worldUI =
        resetTransientUI (worldUI world)
          { uiScreen = MainMenu
          }
    }

-- Level start
startLevel :: Difficulty -> Int -> World -> World
startLevel difficulty idx world =
  case lookup difficulty (worldLevels world) of
    Nothing ->
      world
        { worldUI =
            (worldUI world)
              { uiMessage = Just "Level is not available"
              }
        }

    Just puzzles
      | idx < 0 || idx >= length puzzles ->
          world
            { worldUI =
                (worldUI world)
                  { uiMessage = Just "Puzzle is not available"
                  }
            }

      | otherwise ->
          startLoadedPuzzle (puzzles !! idx) world

startLoadedPuzzle :: LoadedPuzzle -> World -> World
startLoadedPuzzle loaded world =
  world
    { worldGame = gameFromLoaded loaded
    , worldUI =
        resetTransientUI (worldUI world)
          { uiScreen = Playing
          }
    }

-- Hint and hover
showHint :: World -> World
showHint world
  | uiScreen (worldUI world) /= Playing = world
  | uiSolved (worldUI world) = world
  | isSolving world = world
  | otherwise =
      case solveStep (gsCurrent (worldGame world)) of
        Nothing ->
          world
            { worldUI =
                (worldUI world)
                  { uiMessage = Just "No hint available"
                  }
            }

        Just (_, cell, Digit value) ->
          world
            { worldUI =
                (worldUI world)
                  { uiSelected = Just cell
                  , uiMessage = Just ("Hint: place " ++ show value)
                  }
            }

updateHover :: (Float, Float) -> World -> World
updateHover pos world =
  world
    { worldUI =
        (worldUI world)
          { uiHoverButton = buttonAt world pos
          , uiHoverCell = hoverCellAt pos world
          }
    }

hoverCellAt :: (Float, Float) -> World -> Maybe Cell
hoverCellAt pos world =
  case uiScreen (worldUI world) of
    Playing
      | not (isSolving world) -> pointToCell pos
    _ -> Nothing

selectBoardCell :: (Float, Float) -> World -> World
selectBoardCell pos world =
  case pointToCell pos of
    Nothing -> world
    Just c  ->
      let newSelection =
            if uiSelected (worldUI world) == Just c
              then Nothing
              else Just c
       in
      world
        { worldUI =
            (worldUI world)
              { uiSelected = newSelection
              }
        }

-- Keyboard input
handleCharacter :: Char -> World -> World
handleCharacter ch world
  | uiScreen (worldUI world) /= Playing = world
  | uiSolved (worldUI world) = world
  | isSolving world = world
  | otherwise =
      case (uiSelected (worldUI world), ch) of
        (Just cell, '0') ->
          clearCell world cell

        (Just cell, _) ->
          case charToDigit ch of
            Just digit -> applyInput world cell digit
            Nothing -> world

        _ -> world

handleSpecialKey :: Key -> World -> World
handleSpecialKey key world
  | uiScreen (worldUI world) /= Playing = world
  | uiSolved (worldUI world) = world
  | isSolving world = world
  | otherwise =
      case (uiSelected (worldUI world), key) of
        (Just cell, SpecialKey KeyBackspace) ->
          clearCell world cell
        (Just cell, SpecialKey KeyDelete) ->
          clearCell world cell

        _ -> world

-- Time updates
updateWorld :: Float -> World -> World
updateWorld dt world =
  advanceSolveScript dt $
    world { worldUI = updateUIEffects dt (worldUI world) }

advanceSolveScript :: Float -> World -> World
advanceSolveScript dt world =
  case uiSolveScript (worldUI world) of
    [] -> world
    _ ->
      if newTimer > 0
        then
          world
            { worldUI =
                (worldUI world)
                  { uiSolveTimer = newTimer
                  }
            }
        else
          runNextSolveAction world
  where
    newTimer = uiSolveTimer (worldUI world) - dt

runNextSolveAction :: World -> World
runNextSolveAction world =
  case uiSolveScript (worldUI world) of
    [] ->
      world

    action : rest ->
      case action of
        SolveStatus msg delay ->
          world
            { worldUI =
                (worldUI world)
                  { uiSolveScript = rest
                  , uiSolveTimer = delay
                  , uiSelected = Nothing
                  , uiMessage = Just msg
                  }
            }

        SolveMove cell digit ->
          world
            { worldGame =
                (worldGame world)
                  { gsCurrent = boardSet (gsCurrent (worldGame world)) cell digit
                  }
            , worldUI =
                (worldUI world)
                  { uiSolveScript = rest
                  , uiSolveTimer = solveStepDelay
                  , uiSelected = Just cell
                  , uiSolved = False
                  , uiErrorCell = Nothing
                  , uiErrorAlpha = 0.0
                  }
            }

        SolveFinish msg ->
          world
            { worldUI =
                (worldUI world)
                  { uiSolveScript = []
                  , uiSolveTimer = 0.0
                  , uiSolved = True
                  , uiSelected = Nothing
                  , uiMessage = Just msg
                  , uiErrorCell = Nothing
                  , uiErrorAlpha = 0.0
                  , uiSolvedAlpha = 1.0
                  }
            }

updateUIEffects :: Float -> UIState -> UIState
updateUIEffects dt ui =
  ui
    { uiErrorAlpha = newErrorAlpha
    , uiErrorCell = newErrorCell
    , uiSolvedAlpha = newSolvedAlpha
    }
  where
    newErrorAlpha = max 0 (uiErrorAlpha ui - dt * 2.5)

    newErrorCell
      | newErrorAlpha <= 0 = Nothing
      | otherwise = uiErrorCell ui

    newSolvedAlpha
      | uiSolved ui = max 0 (uiSolvedAlpha ui - dt * 0.05)
      | otherwise = 0.0

-- Player input
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

setErrorState :: World -> Cell -> MoveValidity -> World
setErrorState world cell err =
  world
    { worldUI =
        (worldUI world)
          { uiMessage = Just (prettyMoveValidity err)
          , uiErrorCell = Just cell
          , uiErrorAlpha = 1.0
          }
    }

setSolvedState :: World -> GameState -> World
setSolvedState world newGame =
  world
    { worldGame = newGame
    , worldUI =
        resetTransientUI (worldUI world)
          { uiScreen = Playing
          , uiSolved = True
          , uiMessage = Just "Solved!"
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

cellIsImmutable :: GameState -> Cell -> Bool
cellIsImmutable gs (Cell i) = gsGivens gs V.! i

clearCell :: World -> Cell -> World
clearCell world cell =
  let gs = worldGame world
   in if cellIsImmutable gs cell
        then
          setErrorState world cell MoveImmutable
        else
          world
            { worldGame = gs { gsCurrent = boardClear (gsCurrent gs) cell }
            , worldUI = (worldUI world)
                { uiMessage   = Just "Sudoku"
                , uiErrorCell = Nothing
                }
            }

resetCurrentLevel :: World -> World
resetCurrentLevel world
  | uiScreen (worldUI world) /= Playing = world
  | otherwise =
      let gs = worldGame world
       in world
            { worldGame = gs { gsCurrent = gsInitial gs }
            , worldUI =
                resetTransientUI (worldUI world)
                  { uiScreen = Playing
                  }
            }

startSolverAnimation :: World -> World
startSolverAnimation world
  | uiScreen (worldUI world) /= Playing = world
  | uiSolved (worldUI world) = world
  | isSolving world = world
  | otherwise =
      case buildSolveScript (gsCurrent (worldGame world)) of
        [] ->
          world
            { worldUI =
                (worldUI world)
                  { uiMessage = Just "No solver could solve this board"
                  }
            }

        script ->
          world
            { worldUI =
                (worldUI world)
                  { uiSolveScript = script
                  , uiSolveTimer = 0.0
                  , uiSelected = Nothing
                  , uiSolved = False
                  , uiErrorCell = Nothing
                  , uiHoverCell = Nothing
                  , uiErrorAlpha = 0.0
                  , uiSolvedAlpha = 0.0
                  }
            }

-- Solver script
buildSolveScript :: Board -> [SolveAction]
buildSolveScript board =
  buildStrategyScript board solverStrategies

buildStrategyScript :: Board -> [SolverStrategy] -> [SolveAction]
buildStrategyScript _ [] = [SolveFinish "No solver could solve this board"]
buildStrategyScript board [strategy] =
  case solveWithStrategy strategy board of
    Nothing ->
      [ tryingStatus strategy
      , failedSolveStatus strategy
      ]

    Just moves ->
      tryingStatus strategy
      : map (uncurry SolveMove) moves ++
        [ solvedWithStatus strategy ]
buildStrategyScript board (strategy : nextStrategy : rest) =
  case solveWithStrategy strategy board of
    Nothing ->
      tryingStatus strategy
      : nextStrategyStatus strategy nextStrategy
      : buildStrategyScript board (nextStrategy : rest)

    Just moves ->
      tryingStatus strategy
      : map (uncurry SolveMove) moves ++
        [ solvedWithStatus strategy ]

tryingStatus :: SolverStrategy -> SolveAction
tryingStatus strategy =
  SolveStatus ("Trying " ++ strategyLabel strategy ++ " strategy...") statusDelay

nextStrategyStatus :: SolverStrategy -> SolverStrategy -> SolveAction
nextStrategyStatus strategy nextStrategy =
  SolveStatus
    (strategyLabel strategy ++
     " was not enough. Trying " ++
     strategyLabel nextStrategy ++ "...")
    statusDelay

solvedWithStatus :: SolverStrategy -> SolveAction
solvedWithStatus strategy =
  SolveFinish ("Solved using " ++ strategyLabel strategy ++ " strategy")

failedSolveStatus :: SolverStrategy -> SolveAction
failedSolveStatus strategy =
  SolveFinish ("Could not solve using " ++ strategyLabel strategy)

-- Random choice
pickRandomPuzzle :: World -> Maybe (LoadedPuzzle, Int)
pickRandomPuzzle world =
  case concatMap snd (worldLevels world) of
    [] -> Nothing
    puzzles ->
      let gen = worldGenerator world
          (idx, gen') = randomR (0, length puzzles - 1) gen
          seed' = nextGeneratorSeed gen'
       in Just (puzzles !! idx, seed')

worldGenerator :: World -> StdGen
worldGenerator world = mkStdGen (worldRandomSeed world)

nextGeneratorSeed :: StdGen -> Int
nextGeneratorSeed gen =
  fst (randomR (1, 2147483646 :: Int) gen)

-- Theme switch
toggleTheme :: Theme -> Theme
toggleTheme LightTheme = DarkTheme
toggleTheme DarkTheme = LightTheme

-- State reset
resetTransientUI :: UIState -> UIState
resetTransientUI ui =
  ui
    { uiSolved = False
    , uiSelected = Nothing
    , uiHoverCell = Nothing
    , uiHoverButton = Nothing
    , uiMessage = Just "Sudoku"
    , uiErrorCell = Nothing
    , uiErrorAlpha = 0.0
    , uiSolvedAlpha = 0.0
    , uiSolveScript = []
    , uiSolveTimer = 0.0
    }

isSolving :: World -> Bool
isSolving world =
  not (null (uiSolveScript (worldUI world)))

-- User messages
prettyMoveValidity :: MoveValidity -> String
prettyMoveValidity MoveOk = "Sudoku"
prettyMoveValidity MoveConflict = "This move is not allowed"
prettyMoveValidity MoveImmutable = "This cell cannot be changed"
prettyMoveValidity MoveInvalidCell = "Invalid cell"
