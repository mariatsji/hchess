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

gameLoop :: GameHistory -> IO ()
gameLoop gh = do
    putStrLn $ instructions gh
    l <- getLine
    let gameHistory = parseMove l gh
    let newPos = head gameHistory
    Printer.pretty newPos
    putStrLn $ moveOkStatus gh gameHistory
    putStrLn $ statusLine gh
    if null l
        then return()
        else gameLoop gameHistory

instructions gh = (show $ toPlay gh) ++ " to move (e.g. e2-e4) >"

moveOkStatus gh1 gh2 = if (gh1 /= gh2) then "Made move" else "Illegal move"

statusLine gh = if Chess.isCheckMate gh then "Check-Mate to " ++ show (succ' $ toPlay gh) else (show $ succ' $ toPlay gh) ++ " to play"