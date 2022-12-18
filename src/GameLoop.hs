module GameLoop where

import AI (bestDeepEval)
import AppContext (App, AppContext (analysis))
import Chess (Status (BlackToPlay, WhiteToPlay), determineStatus, playIfLegal)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
import Move (parsedMove, playMove)
import Numeric (showFFloat)
import PGN
import Position (
    Color (White),
    Position (m, toPlay),
    findMove,
    startPosition,
 )
import qualified Printer
import System.Exit (exitSuccess)

start :: Text -> App ()
start "1" = do
    Printer.infoTexts ["Examples of moves are e2-e4 O-O-O d7-d8Q", ""]
    gameLoopHH startPosition
start "2" = do
    Printer.infoTexts ["Enter machine search depth (2-5) where 1 is faster and 5 is stronger", ""]
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
start _ = exit

gameLoopHM :: Position -> Int -> App ()
gameLoopHM pos depth = do
    showAnalysis <- asks analysis
    let record p = do
            let filepath = "human-machine-" <> show depth <> ".pgn"
            flightRecorder filepath p
    record pos
    Printer.prettyANSI pos
    l <- Printer.line
    case parsedMove pos l of
        Left e -> do
            Printer.infoTexts ["Could not parse move", e]
            gameLoopHM pos depth
        Right humanMove -> do
            case playIfLegal humanMove pos of
                Left _ -> do
                    Printer.infoTexts ["You can't play " <> show humanMove, "Valid syntax: e2-e4 O-O-O e7-d8Q"]
                    gameLoopHM pos depth
                Right newPos -> do
                    Printer.infoTexts ["thinking..", ""]
                    record newPos
                    Printer.prettyANSI newPos
                    let (aiReplyPosM, scoreM, status') = AI.bestDeepEval newPos depth
                    maybe
                        ( do
                            Printer.infoTexts ["Game over: ", show status']
                            exit
                        )
                        ( \responsePos -> do
                            let move = findMove (m newPos) (m responsePos)
                            Printer.infoTexts [show move, if showAnalysis then prettyScore scoreM else ""]
                            record responsePos
                            Printer.prettyANSI responsePos
                            gameLoopHM responsePos depth
                        )
                        aiReplyPosM

prettyScore :: Maybe Float -> String
prettyScore Nothing = ""
prettyScore (Just s) = formatFloatN s
  where
    formatFloatN floatNum = showFFloat (Just 2) floatNum ""

gameLoopMM :: Position -> Int -> Int -> App ()
gameLoopMM pos whiteDepth blackDepth = do
    showAnalysis <- asks analysis
    let record p = do
            let filepath = "machine-" <> show whiteDepth <> "-machine-" <> show blackDepth <> ".pgn"
            flightRecorder filepath p
    record pos
    Printer.prettyANSI pos
    let depth =
            if toPlay pos == White
                then whiteDepth
                else blackDepth
        (pos', scoreM, status) = AI.bestDeepEval pos depth
    maybe
        ( do
            Printer.infoTexts ["Game over: ", show status]
            exit
        )
        ( \responsePos -> do
            let move = findMove (m pos) (m responsePos)
            Printer.infoTexts [show move, if showAnalysis then prettyScore scoreM else ""]
            record responsePos
            Printer.prettyANSI responsePos
            gameLoopMM responsePos whiteDepth blackDepth
        )
        pos'

gameLoopHH :: Position -> App ()
gameLoopHH pos = do
    let record = flightRecorder "human-vs-human.pgn"
    record pos
    Printer.prettyANSI pos
    l <- Printer.line
    case playMove l pos of
        Left _ -> do
            Printer.infoTexts ["You can't play that.", "Valid syntax: e2-e4 O-O-O e7-d8Q"]
            gameLoopHH pos
        Right pos' ->
            let newStatus = determineStatus pos'
             in if newStatus == WhiteToPlay || newStatus == BlackToPlay
                    then
                        if l == ""
                            then exit
                            else gameLoopHH pos'
                    else do
                        Printer.infoTexts ["Game over: ", show newStatus]
                        flightRecorder "human-vs-human.pgn" pos'
                        exit

exit :: App ()
exit = liftIO exitSuccess

flightRecorder :: FilePath -> Position -> App ()
flightRecorder file pos = liftIO $ do
    let content = renderPgn pos
    TIO.writeFile ("pgn/" <> file) content