module Main where

import AI
import Chess
import Move
import Printer
import System.Mem

main :: IO ()
main = do
    print "1 Human vs Human"
    print "2 Human vs Machine"
    print "3 Machine vs Machine"
    print "q Quit"
    l <- getLine
    start l

start :: String -> IO ()
start "1" = do
  putStrLn "Examples of moves are e2-e4 O-O-O d7-d8Q"
  Printer.pretty Chess.startPosition
  gameLoopHH [Chess.startPosition]
start "2" = do
  putStrLn "Examples of moves are e2-e4 O-O-O d7-d8Q"
  Printer.pretty Chess.startPosition
  gameLoopHM [Chess.startPosition]
start "3" = do
  Printer.pretty Chess.startPosition
  gameLoopMM [Chess.startPosition]
start "q" = return()
start _ = main

gameLoopMM :: GameHistory -> IO ()
gameLoopMM gh = do
  let e = AI.focusedBest gh 3
  case e of Right gameHistory -> do
              let newPos = head gameHistory
              Printer.pretty newPos
              gameLoopMM gameHistory
            Left status -> do
              print status
              return()

gameLoopHM :: GameHistory -> IO ()
gameLoopHM gh = do
  l <- getLine
  let gameHistory = parseMove l gh
  Printer.pretty (head gameHistory)
  putStrLn $ moveOkStatus gh gameHistory
  if determineStatus gameHistory == BlackToPlay then do
    let e = AI.focusedBest gh 2
    case e of Right newGameHistory -> do
                gameLoopHM newGameHistory
              Left status -> do
                print status
                main
  else do
    print $ determineStatus gameHistory
    main

gameLoopHH :: GameHistory -> IO ()
gameLoopHH gh = do
    Printer.pretty (head gh)
    putStrLn "Examples of moves are e2-e4 O-O-O d7-d8Q"
    l <- getLine
    let gameHistory = parseMove l gh
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

moveOkStatus :: GameHistory -> GameHistory -> String
moveOkStatus gh1 gh2 = if validMove gh1 gh2 then "(Ok, a legal move)" else "Illegal move"

validMove :: GameHistory -> GameHistory -> Bool
validMove gh1 gh2 = gh1 /= gh2

toStatus :: Either Status GameHistory -> String
toStatus (Left s) = show s
toStatus (Right gh) = show $ determineStatus gh