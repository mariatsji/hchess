module Main where

import GameLoop
import System.Console.ANSI
import Printer

main :: IO ()
main = do
  clearScreen
  setCursorPosition 0 0
  setTitle "hChess"
  putStrLn "1 Human vs Human"
  putStrLn "2 Human vs Machine"
  putStrLn "3 Machine vs Machine"
  putStrLn "q Quit"
  l <- Printer.line
  start l
