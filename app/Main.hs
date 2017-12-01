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

gameLoopMM :: GameHistory -> IO ()
gameLoopMM gh = do
  putStrLn $ instructions gh
  let gameHistory = AI.first gh
  let newPos = head gameHistory
  Printer.pretty newPos
  putStrLn $ moveOkStatus gh gameHistory
  putStrLn $ statusLine gameHistory
  -- AI reply -- todo only if successful parse!
  let gameHistoryReplied = if validMove gh gameHistory then AI.first gameHistory else gameHistory
  let newPosReplied = head gameHistoryReplied
  Printer.pretty newPosReplied
  gameLoopMM gameHistoryReplied

gameLoopHM :: GameHistory -> IO ()
gameLoopHM gh = do
  putStrLn $ instructions gh
  l <- getLine
  let gameHistory = parseMove l gh
  let newPos = head gameHistory
  Printer.pretty newPos
  putStrLn $ moveOkStatus gh gameHistory
  putStrLn $ statusLine gameHistory
  -- AI reply -- todo only if successful parse!
  let gameHistoryReplied = if validMove gh gameHistory then AI.first gameHistory else gameHistory
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
    putStrLn $ statusLine gameHistory
    if null l
        then return()
        else gameLoopHH gameHistory

instructions gh = (show $ toPlay gh) ++ " to move (e.g. e2-e4) >"

moveOkStatus gh1 gh2 = if validMove gh1 gh2 then "Made move" else "Illegal move"

validMove gh1 gh2 = gh1 /= gh2

statusLine gh = if Chess.isCheckMate gh
  then "Check-Mate to " ++ show (succ' $ toPlay gh)
  else
    if Chess.isDraw gh
      then "Stalemate!"
      else (show $ succ' $ toPlay gh) ++ " to play"