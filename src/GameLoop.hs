module GameLoop where

import AI (edgeGreed)
import Chess (Status (BlackToPlay, WhiteToPlay), determineStatus, playMove')
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Move (parsedMove, playMove)
import Position (
    Color (White),
    Move,
    Position (m, toPlay, gamehistory),
    findMove,
    startPosition,
 )
import Printer (pretty)
import System.Exit (exitSuccess)

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
    gameLoopHM [] startPosition depth
start "3" = do
    putStrLn "Enter white search depth (2-5) where 2 is faster and 5 is stronger"
    lw <- getLine
    let wdepth = read lw :: Int
    putStrLn "Enter black search depth (2-5) where 2 is faster and 5 is stronger"
    lb <- getLine
    let bdepth = read lb :: Int
    Printer.pretty startPosition
    gameLoopMM startPosition wdepth bdepth
start _ = exitSuccess

gameLoopMM :: Position -> Int -> Int -> IO ()
gameLoopMM pos whiteDepth blackDepth = do
    let depth =
            if toPlay pos == White
                then whiteDepth
                else blackDepth
    case AI.edgeGreed pos depth of
        Right pos' -> do
            let move = findMove (m pos) (m pos')
            Printer.pretty pos'
            gameLoopMM pos' whiteDepth blackDepth
        Left (pos', status) -> do
            print status
            Printer.pretty pos'
            pure ()

gameLoopHM :: [Move] -> Position -> Int -> IO ()
gameLoopHM moves pos depth = do
    l <- TIO.getLine
    case parsedMove pos l of
        Left e -> do
            putStrLn $ "Could not parse move: " <> e
            gameLoopHM moves pos depth
        Right humanMove -> do
            --todo if findMove elem legalMoves blabalalb
            case playMove' humanMove pos of
                Left e -> do
                    putStrLn $ "Could not play human move: " <> e
                    gameLoopHM moves pos depth
                Right newPos -> do
                    Printer.pretty newPos
                    let status = determineStatus newPos
                    print $ "determine status : " <> show status
                    if status == BlackToPlay
                        then case AI.edgeGreed newPos depth of
                            Right newPos2 -> do
                                print$  "edgeGreed result Right: " <> show (length (gamehistory newPos2))
                                let move = findMove (m newPos) (m newPos2)
                                print move
                                Printer.pretty newPos2
                                gameLoopHM (move : humanMove : moves) newPos2 depth
                            Left (pos'', status') -> do
                                print$  "edgeGreed result Left: " <> show (length (gamehistory pos''))
                                Printer.pretty pos''
                                print status'
                                flightRecorder moves
                                exitSuccess
                        else do
                            print status
                            gameLoopHM (humanMove : moves) pos depth

gameLoopHH :: Position -> IO ()
gameLoopHH pos = do
    Printer.pretty pos
    putStrLn "Examples of moves are e2-e4 O-O-O d7-d8Q or newline to quit to menu"
    l <- getLine
    case playMove l pos of
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

flightRecorder :: [Move] -> IO ()
flightRecorder moves = do
    print "saving game.log"
    let content = show $ reverse moves
    TIO.writeFile "game.log" (pack content)