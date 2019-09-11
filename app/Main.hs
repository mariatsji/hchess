{-# LANGUAGE BangPatterns #-}

module Main where

import AI
import Chess
import Control.DeepSeq
import Move
import Position
import Printer

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
  Printer.pretty startPosition
  gameLoopHH startPosition
start "2" = do
  putStrLn "Examples of moves are e2-e4 O-O-O d7-d8Q"
  putStrLn
    "Enter machine search depth (2-5) where 1 is faster and 5 is stronger"
  l <- getLine
  let depth = read l :: Int
  Printer.pretty startPosition
  gameLoopHM startPosition depth
start "3" = do
  putStrLn "Enter white search depth (2-5) where 2 is faster and 5 is stronger"
  lw <- getLine
  let wdepth = read lw :: Int
  putStrLn "Enter black search depth (2-5) where 2 is faster and 5 is stronger"
  lb <- getLine
  let bdepth = read lb :: Int
  Printer.pretty startPosition
  gameLoopMM startPosition wdepth bdepth
start "q" = return ()
start _ = main

gameLoopMM :: Position -> Int -> Int -> IO ()
gameLoopMM !pos whiteDepth blackDepth = do
  let depth =
        if toPlay pos == White
          then whiteDepth
          else blackDepth
  case force $ AI.streamBest pos depth of
    Right pos' -> do
      Printer.pretty pos'
      gameLoopMM pos' whiteDepth blackDepth
    Left (pos', status) -> do
      print status
      Printer.pretty pos'
      main

gameLoopHM :: Position -> Int -> IO ()
gameLoopHM !pos depth = do
  l <- getLine
  case parseMove l pos of
    Left s -> do
      putStrLn "Could not parse move"
      gameLoopHM pos depth
    Right newPos -> do
      Printer.pretty newPos
      let status = determineStatus newPos
      if status == BlackToPlay
        then case force $ AI.streamBest newPos depth of
          Right newPos2 -> do
            Printer.pretty newPos2
            gameLoopHM newPos2 depth
          Left (pos'', status) -> do
            Printer.pretty pos''
            print status
            main
        else do
          print status
          gameLoopHM pos depth

gameLoopHH :: Position -> IO ()
gameLoopHH !pos = do
  Printer.pretty pos
  putStrLn "Examples of moves are e2-e4 O-O-O d7-d8Q or newline to quit to menu"
  l <- getLine
  case parseMove l pos of
    Left s -> do
      print $ "could not parse " ++ s
      gameLoopHH pos
    Right pos' ->
      let newStatus = determineStatus pos'
       in if newStatus == WhiteToPlay || newStatus == BlackToPlay
            then
              if null l
                then main
                else gameLoopHH pos'
            else do
              print newStatus
              main
