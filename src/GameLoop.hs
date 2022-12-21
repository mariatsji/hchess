module GameLoop where

import AI (bestDeepEval)
import AppContext (App, World (..))
import Chess (Status (BlackToPlay, WhiteToPlay), determineStatus, playIfLegal, positionTree)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import Move (parsedMove, playMove)
import Numeric (showFFloat)
import PGN
import Position (
    Color (White),
    Position (toPlay),
    startPosition,
 )
import qualified Printer
import System.Exit (exitSuccess)

start :: Text -> App ()
start "1" = do
    Printer.infoTexts ["Examples of moves are e2-e4 O-O-O d7-d8Q", ""]
    gameLoopHH startPosition
start "2" = do
    Printer.infoTexts ["Enter machine search depth (1-3) where 1 is faster and 3 is stronger", ""]
    l <- Printer.line
    let depth = read @Int (unpack l)
    gameLoopHM startPosition depth
start "3" = do
    Printer.infoTexts ["Enter white search depth (1-3) where 2 is faster and 3 is stronger", ""]
    lw <- Printer.line
    let wdepth = read @Int (unpack lw)
    Printer.infoTexts ["Enter black search depth (1-3) where 2 is faster and 3 is stronger", ""]
    lb <- Printer.line
    let bdepth = read @Int (unpack lb)
    gameLoopMM startPosition wdepth bdepth
start _ = exit

gameLoopHM :: Position -> Int -> App ()
gameLoopHM pos depth = do
    let record p = do
            let filepath = "human-machine-" <> show depth <> ".pgn"
            flightRecorder filepath p
    record pos
    let world =
            World
                { wTitle = "Human player vs Machine at depth " <> showt depth
                , wPos = Just pos
                , wScore = Nothing
                , wInfo = []
                }
    Printer.render world
    l <- Printer.line
    case parsedMove pos l of
        Left e -> do
            Printer.render world {wInfo = ["Could not parse move", showt e]}
            gameLoopHM pos depth
        Right humanMove -> do
            case playIfLegal humanMove pos of
                Left _ -> do
                    Printer.render world {wInfo = ["You can't play " <> showt humanMove, "Valid syntax: e2-e4 O-O-O e7-d8Q"]}
                    gameLoopHM pos depth
                Right newPos -> do
                    record newPos
                    Printer.render world {wInfo = ["thinking..", ""], wPos = Just newPos}
                    let (aiReplyPosM, scoreM, status') = AI.bestDeepEval newPos depth
                    maybe
                        ( do
                            Printer.render world {wInfo = ["Game over: ", showt status']}
                            exit
                        )
                        ( \responsePos -> do
                            Printer.render world {wInfo = ["White's move",""], wPos = Just responsePos, wScore = scoreM}
                            record responsePos
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
    let record p = do
            let filepath = "machine-" <> show whiteDepth <> "-machine-" <> show blackDepth <> ".pgn"
            flightRecorder filepath p
    record pos
    let world =
            World
                { wTitle = "machine at depth " <> showt whiteDepth <> " vs machine at depth " <> showt blackDepth
                , wPos = Just pos
                , wScore = Nothing
                , wInfo = []
                }
    Printer.render world
    let depth =
            if toPlay pos == White
                then whiteDepth
                else blackDepth
        (pos', scoreM, status) = AI.bestDeepEval pos depth
    maybe
        ( do
            Printer.infoTexts ["Game over: ", showt status]
            exit
        )
        ( \responsePos -> do
            record responsePos
            Printer.render world {wPos = Just responsePos, wScore = scoreM}
            gameLoopMM responsePos whiteDepth blackDepth
        )
        pos'

gameLoopHH :: Position -> App ()
gameLoopHH pos = do
    let record = flightRecorder "human-vs-human.pgn"
    record pos
    let world =
            World
                { wTitle = "human-human"
                , wPos = Just pos
                , wScore = Nothing
                , wInfo = []
                }
    Printer.render world
    l <- Printer.line
    case playMove l pos of
        Left _ -> do
            Printer.render world {wInfo = ["You can't play that.", "Valid syntax: e2-e4 O-O-O e7-d8Q"]}
            gameLoopHH pos
        Right pos' ->
            let newStatus = determineStatus pos' (positionTree pos')
             in if newStatus == WhiteToPlay || newStatus == BlackToPlay
                    then
                        if l == ""
                            then exit
                            else gameLoopHH pos'
                    else do
                        Printer.render world {wInfo = ["Game over: ", showt newStatus]}
                        flightRecorder "human-vs-human.pgn" pos'
                        exit

showt :: Show a => a -> Text
showt = pack . show

exit :: App ()
exit = liftIO exitSuccess

flightRecorder :: FilePath -> Position -> App ()
flightRecorder file pos = liftIO $ do
    let content = renderPgn pos
    TIO.writeFile ("pgn/" <> file) content