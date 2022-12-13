module GameLoop where

import AI (edgeGreed)
import Chess (Status (BlackToPlay, WhiteToPlay), determineStatus, playMove', positionTree)
import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Move (parsedMove, playMove)
import Position (
    Color (White),
    Move,
    Position (gamehistory, m, toPlay),
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
    putStrLn "Clearing earlier game.log"
    TIO.writeFile "game.log" ""
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

gameLoopHM :: Position -> Int -> IO ()
gameLoopHM pos depth = do
    l <- TIO.getLine
    case parsedMove pos l of
        Left e -> do
            putStrLn $ "Could not parse move: " <> e
            gameLoopHM pos depth
        Right humanMove -> do
            case playIfLegal humanMove pos of
                Left e -> do
                    putStrLn $ "You can't play this: " <> e
                    gameLoopHM pos depth
                Right newPos -> do
                    flightRecorderAppend humanMove
                    Printer.pretty newPos
                    let status = determineStatus newPos
                    print $ "determine status : " <> show status
                    if status == BlackToPlay
                        then case AI.edgeGreed newPos depth of
                            Right newPos2 -> do
                                let move = findMove (m newPos) (m newPos2)
                                flightRecorderAppend move
                                print move
                                Printer.pretty newPos2
                                gameLoopHM newPos2 depth
                            Left (pos'', status') -> do
                                Printer.pretty pos''
                                print status'
                                exitSuccess
                        else do
                            print status
                            gameLoopHM pos depth

playIfLegal :: Move -> Position -> Either String Position
playIfLegal move pos =
    let legalMoves = positionTree pos
     in playMove' move pos
            >>= \p -> if p `elem` legalMoves then Right p else Left (show move <> " is ot a legal move in position")

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

flightRecorderAppend :: Move -> IO ()
flightRecorderAppend move = do
    let filename = "game.log"
    print $ "saving" <> filename
    existing <- TIO.readFile filename
    TIO.writeFile filename (existing <> "\n" <> pack (show move))