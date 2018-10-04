module Main where

import           AI
import           Chess
import           Move
import           Printer

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
  putStrLn
    "Enter machine search depth (2-5) where 1 is faster and 5 is stronger"
  l <- getLine
  let depth = read l :: Int
  Printer.pretty Chess.startPosition
  gameLoopHM [Chess.startPosition] depth
start "3" = do
  putStrLn "Enter white search depth (2-5) where 2 is faster and 5 is stronger"
  lw <- getLine
  let wdepth = read lw :: Int
  putStrLn "Enter black search depth (2-5) where 2 is faster and 5 is stronger"
  lb <- getLine
  let bdepth = read lb :: Int
  Printer.pretty Chess.startPosition
  gameLoopMM [Chess.startPosition] wdepth bdepth
start "q" = return ()
start _ = main

gameLoopMM :: GameHistory -> Int -> Int -> IO ()
gameLoopMM gh whiteDepth blackDepth = do
  let depth =
        if toPlay gh == White
          then whiteDepth
          else blackDepth
  let e = AI.edgeGreed gh depth
  case e of
    Right gameHistory -> do
      let newPos = head gameHistory
      Printer.pretty newPos
      gameLoopMM gameHistory whiteDepth blackDepth
    Left (gh'', status) -> do
      Printer.pretty (head gh'')
      print status
      return ()

gameLoopHM :: GameHistory -> Int -> IO ()
gameLoopHM gh depth = do
  l <- getLine
  let gameHistory = parseMove l gh
  putStrLn $ moveOkStatus gh gameHistory
  Printer.pretty (head gameHistory)
  if determineStatus gameHistory == BlackToPlay
    then do
      let e = AI.edgeGreed gameHistory depth
      case e of
        Right newGameHistory -> do
          Printer.pretty (head newGameHistory)
          gameLoopHM newGameHistory depth
        Left (gh'', status) -> do
          Printer.pretty (head gh'')
          print status
          main
    else do
      print $ determineStatus gameHistory
      gameLoopHM gh depth

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
           then return ()
           else gameLoopHH gameHistory
    else do
      print newStatus
      main

moveOkStatus :: GameHistory -> GameHistory -> String
moveOkStatus gh1 gh2 =
  if validMove gh1 gh2
    then "(Ok, a legal move)"
    else "Illegal move"

validMove :: GameHistory -> GameHistory -> Bool
validMove gh1 gh2 = gh1 /= gh2

toStatus :: Either Status GameHistory -> String
toStatus (Left s)   = show s
toStatus (Right gh) = show $ determineStatus gh
