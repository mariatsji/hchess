module Main where

import Chess
import Printer

main :: IO ()
main = Printer.pretty Chess.startPosition
