module GameLoop where

import AI (bestDeepEval)
import Chess (Status (BlackToPlay, WhiteToPlay), determineStatus, playIfLegal)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
import Move (parsedMove, playMove)
import Position (
    Color (White),
    Position (m, toPlay),
    findMove,
    startPosition,
 )
import PGN
import qualified Printer
import System.Exit (exitSuccess)

start :: Text -> IO ()
start "1" = do
    Printer.infoTexts ["Examples of moves are e2-e4 O-O-O d7-d8Q", ""]
    gameLoopHH startPosition
start "2" = do
    Printer.infoTexts [ "Enter machine search depth (2-5) where 1 is faster and 5 is stronger" , "" ]
    l <- Printer.line
    let depth = read @Int (unpack l)
    gameLoopHM startPosition depth
start "3" = do
    Printer.infoTexts ["Enter white search depth (2-5) where 2 is faster and 5 is stronger", ""]
    lw <- Printer.line
    let wdepth = read @Int (unpack lw)
    Printer.infoTexts ["Enter black search depth (2-5) where 2 is faster and 5 is stronger", ""]
    lb <- Printer.line
    let bdepth = read @Int (unpack lb)
    gameLoopMM startPosition wdepth bdepth
start _ = exitSuccess

gameLoopHM :: Position -> Int -> IO ()
gameLoopHM pos depth = do
    Printer.prettyANSI pos
    l <- Printer.line
    case parsedMove pos l of
        Left e -> do
            Printer.infoTexts ["Could not parse move", e]
            gameLoopHM pos depth
        Right humanMove -> do
            case playIfLegal humanMove pos of
                Left _ -> do
                    Printer.infoTexts ["You can't play " <> show humanMove, "Valid examples: e2-e4 O-O-O e7-d8Q"]
                    gameLoopHM pos depth
                Right newPos -> do
                    Printer.infoTexts ["thinking..", ""]
                    Printer.prettyANSI newPos
                    let (aiReplyPosM, status') = AI.bestDeepEval newPos depth
                    maybe
                        ( do
                            Printer.infoTexts ["Game over: ", show status']
                            flightRecorder "human-vs-machine.pgn" newPos
                            exitSuccess
                        )
                        ( \responsePos -> do
                            let move = findMove (m newPos) (m responsePos)
                            Printer.infoTexts [show move, ""]
                            Printer.prettyANSI responsePos
                            gameLoopHM responsePos depth
                        )
                        aiReplyPosM

gameLoopMM :: Position -> Int -> Int -> IO ()
gameLoopMM pos whiteDepth blackDepth = do
    Printer.prettyANSI pos
    let depth =
            if toPlay pos == White
                then whiteDepth
                else blackDepth
        (pos', status) = AI.bestDeepEval pos depth
    maybe
        ( do
            Printer.infoTexts ["Game over: ", show status]
            let filepath = "machine-" <> show whiteDepth <> "-machine-" <> show blackDepth <> ".pgn"
            flightRecorder filepath pos
            exitSuccess
        )
        ( \responsePos -> do
            let move = findMove (m pos) (m responsePos)
            Printer.infoTexts [show move, ""]
            Printer.prettyANSI responsePos
            gameLoopMM responsePos whiteDepth blackDepth
        )
        pos'

gameLoopHH :: Position -> IO ()
gameLoopHH pos = do
    Printer.prettyANSI pos
    l <- Printer.line
    case playMove l pos of
        Left _ -> do
            Printer.infoTexts ["You can't play that.", "Valid examples: e2-e4 O-O-O e7-d8Q"]
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
                        flightRecorder "human-vs-human.pgn" pos'
                        exitSuccess

flightRecorder :: FilePath -> Position -> IO ()
flightRecorder file pos = do
    let content = renderPgn pos
    TIO.writeFile file content