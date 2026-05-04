module Assets
  ( loadUIAssets
  ) where

import Graphics.Gloss.Data.Bitmap (loadBMP)

import Types

-- Bitmap loader
loadUIAssets :: IO UIAssets
loadUIAssets =
  UIAssets
    <$> loadBMP "assets/bulb.bmp"
    <*> loadBMP "assets/bulb_white.bmp"
    <*> loadBMP "assets/circle-half-stroke.bmp"
    <*> loadBMP "assets/circle-half-stroke_white.bmp"
    <*> loadBMP "assets/dice-three.bmp"
    <*> loadBMP "assets/dice_three_white.bmp"
    <*> loadBMP "assets/rotate-left.bmp"
    <*> loadBMP "assets/rotate-left_white.bmp"
    <*> loadBMP "assets/refresh.bmp"
    <*> loadBMP "assets/refresh_white.bmp"
