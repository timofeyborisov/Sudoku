module Main where

import Types

main :: IO ()
main = do
  putStrLn "Sudoku project skeleton is ready."
  putStrLn "Modules: Types, Board, Levels."
  print (Nothing :: Maybe Cell)
