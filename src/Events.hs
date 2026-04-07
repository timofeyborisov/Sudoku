module Events
  ( handleEvent
  , updateWorld
  ) where

import Graphics.Gloss.Interface.Pure.Game
import Types

handleEvent :: Event -> World -> World
handleEvent _ world = world

updateWorld :: Float -> World -> World
updateWorld _ world = world