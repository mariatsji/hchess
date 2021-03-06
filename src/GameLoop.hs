module GameLoop where

import AI
import Chess
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Move
import Position
import Printer
import System.Exit

type App a = WriterT [Move] IO a

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
  moves <- runWriterT $ gameLoopMM startPosition wdepth bdepth
  print moves
start _ = exitSuccess

gameLoopMM :: Position -> Int -> Int -> App ()
gameLoopMM pos whiteDepth blackDepth = do
  let depth =
        if toPlay pos == White
          then whiteDepth
          else blackDepth
  case AI.edgeGreed pos depth of
    Right pos' -> do
      tell [findMove (m pos) (m pos')]
      liftIO $ Printer.pretty pos'
      gameLoopMM pos' whiteDepth blackDepth
    Left (pos', status) -> do
      liftIO $ print status
      liftIO $ Printer.pretty pos'
      pure ()

gameLoopHM :: Position -> Int -> IO ()
gameLoopHM pos depth = do
  l <- getLine
  case parseMove l pos of
    Left _ -> do
      putStrLn "Could not parse move"
      gameLoopHM pos depth
    Right newPos -> do
      Printer.pretty newPos
      let status = determineStatus newPos
      if status == BlackToPlay
        then case AI.edgeGreed newPos depth of
          Right newPos2 -> do
            Printer.pretty newPos2
            gameLoopHM newPos2 depth
          Left (pos'', status') -> do
            Printer.pretty pos''
            print status'
            exitSuccess
        else do
          print status
          gameLoopHM pos depth

gameLoopHH :: Position -> IO ()
gameLoopHH pos = do
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
                then exitSuccess
                else gameLoopHH pos'
            else do
              print newStatus
              exitSuccess
