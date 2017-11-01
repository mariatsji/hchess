module Main where

import Lib
import Chess
import Printer

main :: IO ()
main = Printer.pretty Chess.startPosition
