module Main where

import Chess
import Data.Char
import Data.Maybe
import Move
import Printer
import System.IO

main :: IO ()
main = do
    Printer.pretty Chess.startPosition
    gameLoop [Chess.startPosition]

gameLoop :: [Position] -> IO ()
gameLoop pos = do
    putStrLn "Enter move (e.g. e2-e4) >"
    l <- getLine
    let newPosList = parseMove l pos
    let newPos = head newPosList
    Printer.pretty newPos
    if null l
        then return()
        else gameLoop newPosList