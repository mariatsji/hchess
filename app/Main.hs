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
    print "q Quit"
    l <- getLine
    start l

start :: String -> IO ()
start "1" = gameLoopHH [Chess.startPosition]
start "2" = gameLoopHM [Chess.startPosition]
start "3" = gameLoopMM [Chess.startPosition]
start "q" = return()
start _ = main

gameLoopMM :: GameHistory -> IO ()
gameLoopMM gh = do
  let e = AI.best gh 3
  case e of Right gameHistory -> do
              let newPos = head gameHistory
              Printer.pretty newPos
              gameLoopMM gameHistory
            Left status -> do
              print status
              return()

gameLoopHM :: GameHistory -> IO ()
gameLoopHM gh = do
  Printer.pretty (head gh)
  putStrLn "Examples of moves are e2-e4 O-O-O d7-d8Q"
  l <- getLine
  let gameHistory = parseMove l gh
  let newPos = head gameHistory
  putStrLn $ moveOkStatus gh gameHistory
  let newStatus = determineStatus gameHistory
  print newStatus
  if newStatus == BlackToPlay then do
    let e = AI.best gh 3
    case e of Right gameHistory -> do
                gameLoopHM gameHistory
              Left status -> do
                 print status
                 main
  else do
    print newStatus
    main

gameLoopHH :: GameHistory -> IO ()
gameLoopHH gh = do
    Printer.pretty (head gh)
    putStrLn "Examples of moves are e2-e4 O-O-O d7-d8Q"
    l <- getLine
    let gameHistory = parseMove l gh
    let newPos = head gameHistory
    putStrLn $ moveOkStatus gh gameHistory
    let newStatus = determineStatus gameHistory
    print newStatus
    if newStatus == WhiteToPlay || newStatus == BlackToPlay
      then if null l
        then return()
        else gameLoopHH gameHistory
      else
        do
          print newStatus
          main

moveOkStatus gh1 gh2 = if validMove gh1 gh2 then "(Ok, a legal move)" else "Illegal move"

validMove gh1 gh2 = gh1 /= gh2
