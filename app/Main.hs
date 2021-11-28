module Main where

import GameLoop

main :: IO ()
main = do
  putStrLn "1 Human vs Human"
  putStrLn "2 Human vs Machine"
  putStrLn "3 Machine vs Machine"
  putStrLn "q Quit"
  l <- getLine
  start l

