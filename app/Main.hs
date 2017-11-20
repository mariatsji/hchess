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
    putStrLn "Enter move (e.g. e2-e4) >"
    l <- getLine
    let gameHistory = parseMove l gh
    let moveOkStatus = if (gameHistory /= gh) then "Made move" else "Illegal move"
    let newPos = head gameHistory
    Printer.pretty newPos
    putStrLn moveOkStatus
    let statusLine = if Chess.isCheckMate gameHistory
        then
          "Check-Mate to " ++ show (succ' $ toPlay gameHistory)
        else
          (show $ succ' $ toPlay gameHistory) ++ " to play"
    putStrLn statusLine
    if null l
        then return()
        else gameLoop gameHistory