module GameLoop where

import AI (edgeGreed)
import Chess (Status (BlackToPlay, WhiteToPlay), determineStatus, playIfLegal, positionTree)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import Move (parsedMove, playMove)
import Position (
    Color (White),
    Move,
    Position (gamehistory, m, toPlay),
    findMove,
    startPosition,
 )
import qualified Printer
import qualified System.Console.ANSI as ANSI
import System.Exit (exitSuccess)
import Evaluation (terminal)

start :: Text -> IO ()
start "1" = do
    Printer.infoTexts ["Examples of moves are e2-e4 O-O-O d7-d8Q", ""]
    Printer.prettyANSI startPosition
    gameLoopHH startPosition
start "2" = do
    Printer.infoTexts
        [ "Examples of moves are e2-e4 O-O-O d7-d8Q"
        , "Enter machine search depth (2-5) where 1 is faster and 5 is stronger"
        ]
    l <- Printer.line
    let depth = read @Int (unpack l)
    TIO.writeFile "game.log" ""
    Printer.prettyANSI startPosition
    Printer.clearInfo
    gameLoopHM startPosition depth
start "3" = do
    Printer.infoTexts ["Enter white search depth (2-5) where 2 is faster and 5 is stronger", ""]
    lw <- Printer.line
    let wdepth = read @Int (unpack lw)
    Printer.infoTexts ["Enter black search depth (2-5) where 2 is faster and 5 is stronger", ""]
    lb <- Printer.line
    let bdepth = read @Int (unpack lb)
    Printer.prettyANSI startPosition
    gameLoopMM startPosition wdepth bdepth
start _ = exitSuccess

gameLoopMM :: Position -> Int -> Int -> IO ()
gameLoopMM pos whiteDepth blackDepth = do
    let depth =
            if toPlay pos == White
                then whiteDepth
                else blackDepth
    let (pos', status) = AI.edgeGreed pos depth
    if terminal status then do
        Printer.infoTexts ["Game over: ", show status]
        Printer.prettyANSI pos'
        pure ()
    else do
        let move = findMove (m pos) (m pos')
        Printer.prettyANSI pos'
        gameLoopMM pos' whiteDepth blackDepth
            

gameLoopHM :: Position -> Int -> IO ()
gameLoopHM pos depth = do
    l <- Printer.line
    Printer.clearInfo
    case parsedMove pos l of
        Left e -> do
            Printer.infoTexts ["Could not parse move", e]
            gameLoopHM pos depth
        Right humanMove -> do
            case playIfLegal humanMove pos of
                Left e -> do
                    Printer.infoTexts ["You can't play " <> show humanMove, e]
                    gameLoopHM pos depth
                Right newPos -> do
                    flightRecorderAppend humanMove
                    Printer.infoTexts ["thinking..", ""]
                    Printer.prettyANSI newPos
                    let (newPos2, status') = AI.edgeGreed newPos depth
                    if terminal status' then do
                        Printer.prettyANSI newPos2
                        Printer.infoTexts ["Game over: ", show status']
                        exitSuccess
                    else do
                        let move = findMove (m newPos) (m newPos2)
                        flightRecorderAppend move
                        Printer.infoTexts [show move, ""]
                        Printer.prettyANSI newPos2
                        gameLoopHM newPos2 depth
                            

gameLoopHH :: Position -> IO ()
gameLoopHH pos = do
    Printer.prettyANSI pos
    Printer.infoTexts ["Examples of moves are e2-e4 O-O-O d7-d8Q", ""]
    l <- Printer.line
    case playMove l pos of
        Left s -> do
            Printer.infoTexts ["could not parse ", s]
            gameLoopHH pos
        Right pos' ->
            let newStatus = determineStatus pos'
             in if newStatus == WhiteToPlay || newStatus == BlackToPlay
                    then
                        if l == ""
                            then exitSuccess
                            else gameLoopHH pos'
                    else do
                        Printer.infoTexts ["Game over: ", show newStatus]
                        exitSuccess

flightRecorderAppend :: Move -> IO ()
flightRecorderAppend move = do
    let filename = "game.log"
    existing <- TIO.readFile filename
    TIO.writeFile filename (existing <> "\n" <> pack (show move))