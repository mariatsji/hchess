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
    print "1 Human vs Human"
    print "2 Human vs Machine"
    print "3 Machine vs Machine"
    l <- getLine
    start l

start :: String -> IO ()
start "1" = gameLoopHH [Chess.startPosition]
start "2" = gameLoopHM [Chess.startPosition]
start "3" = gameLoopMM [Chess.startPosition]
start _ = main

gameLoopMM :: GameHistory -> IO ()
gameLoopMM gh = do
  let gameHistory = AI.first gh
  let newPos = head gameHistory
  Printer.pretty newPos
  let newStatus = status gameHistory
  print newStatus
  if newStatus == Remis || newStatus == WhiteIsMate || newStatus == BlackIsMate
    then
      return()
    else
      gameLoopMM gameHistory

gameLoopHM :: GameHistory -> IO ()
gameLoopHM gh = do
  Printer.pretty (head gh)
  putStrLn "Examples of moves are e2-e4 O-O-O d7-d8Q"
  l <- getLine
  let gameHistory = parseMove l gh
  let newPos = head gameHistory
  putStrLn $ moveOkStatus gh gameHistory
  let newStatus = status gameHistory
  print newStatus
  -- AI reply -- todo only if successful parse!
  let gameHistoryReplied = if validMove gh gameHistory then AI.first gameHistory else gameHistory
  let newPosReplied = head gameHistoryReplied
  Printer.pretty newPosReplied
  if null l
      then return()
      else gameLoopHM gameHistoryReplied

gameLoopHH :: GameHistory -> IO ()
gameLoopHH gh = do
    Printer.pretty (head gh)
    putStrLn "Examples of moves are e2-e4 O-O-O d7-d8Q"
    l <- getLine
    let gameHistory = parseMove l gh
    let newPos = head gameHistory
    putStrLn $ moveOkStatus gh gameHistory
    let newStatus = status gameHistory
    print newStatus
    if newStatus == WhiteToPlay || newStatus == BlackToPlay
      then if null l
        then return()
        else gameLoopHH gameHistory
      else
        do
          print newStatus
          main

moveOkStatus gh1 gh2 = if validMove gh1 gh2 then "Made move" else "Illegal move"

validMove gh1 gh2 = gh1 /= gh2

status :: GameHistory -> Status
status gh = if Chess.isCheckMate gh then
  if Chess.toPlay gh == Black then BlackIsMate else WhiteIsMate
  else
    if Chess.isDraw gh then Remis
    else if Chess.toPlay gh == White then WhiteToPlay else BlackToPlay
