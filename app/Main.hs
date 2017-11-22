module Main where

import AI
import Chess
import Move
import Printer

import Data.Char
import Data.Maybe
import System.IO

main :: IO ()
main = do
    Printer.pretty Chess.startPosition
    gameLoopHM [Chess.startPosition]

gameLoopHM :: GameHistory -> IO ()
gameLoopHM gh = do
  putStrLn $ instructions gh
  l <- getLine
  let gameHistory = parseMove l gh
  let newPos = head gameHistory
  Printer.pretty newPos
  putStrLn $ moveOkStatus gh gameHistory
  putStrLn $ statusLine gh
  -- AI reply -- todo only if successful parse!
  let gameHistoryReplied = AI.first gameHistory
  let newPosReplied = head gameHistoryReplied
  Printer.pretty newPosReplied
  if null l
      then return()
      else gameLoopHM gameHistoryReplied

gameLoopHH :: GameHistory -> IO ()
gameLoopHH gh = do
    putStrLn $ instructions gh
    l <- getLine
    let gameHistory = parseMove l gh
    let newPos = head gameHistory
    Printer.pretty newPos
    putStrLn $ moveOkStatus gh gameHistory
    putStrLn $ statusLine gh
    if null l
        then return()
        else gameLoopHH gameHistory

instructions gh = (show $ toPlay gh) ++ " to move (e.g. e2-e4) >"

moveOkStatus gh1 gh2 = if (gh1 /= gh2) then "Made move" else "Illegal move"

statusLine gh = if Chess.isCheckMate gh
  then "Check-Mate to " ++ show (succ' $ toPlay gh)
  else
    if Chess.isPatt gh
      then "Stalemate!"
      else (show $ succ' $ toPlay gh) ++ " to play"