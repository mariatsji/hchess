module GameLoop where

import AI (bestDeepEval)
import AppContext (App, AppContext (blackDepth, perspective, whiteDepth, startFrom), World (..))
import Chess (Status (BlackToPlay, WhiteToPlay), determineStatus, playIfLegal, positionTree)
import Evaluation (terminal)
import Move (parsedMove, playMove)
import PGN
import Position (
    Color (Black, White),
    Position (toPlay),
    startPosition,
 )
import qualified Printer

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Numeric (showFFloat)
import Data.Maybe (fromMaybe)
import Relude

start :: Text -> App ()
start "1" = do
    Printer.clearTopScreen
    Printer.infoTexts ["Examples of moves are e2-e4 O-O-O d7-d8Q", ""]
    pos <- asks startFrom
    gameLoopHH $ fromMaybe startPosition pos
start "2" = do
    perspective' <- asks perspective
    pos <- asks startFrom
    Printer.clearTopScreen
    case perspective' of
        White -> do
            depth <- asks whiteDepth
            gameLoopHM (fromMaybe startPosition pos) depth
        Black -> do
            depth <- asks blackDepth
            let (Just opening, _, _) = AI.bestDeepEval (fromMaybe startPosition pos) depth
            gameLoopHM opening depth
start "3" = do
    Printer.clearTopScreen
    wdepth <- asks whiteDepth
    bdepth <- asks blackDepth
    pos <- asks startFrom
    gameLoopMM (fromMaybe startPosition pos) wdepth bdepth
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
    if l == "resign" then do
        Printer.render world {wInfo = ["Game over: ", "computer wins"]}
        exit
    else do
        case parsedMove pos l of
            Left e -> do
                Printer.render world {wInfo = ["Could not parse move", showt e]}
                gameLoopHM pos depth
            Right humanMove -> do
                case playIfLegal humanMove pos of
                    Left s -> do
                        Printer.render world {wInfo = [showt humanMove <> " not playable: " <> pack s, "example of syntax: e2-e4 h7-h8Q"]}
                        gameLoopHM pos depth
                    Right newPos -> do
                        record newPos
                        Printer.render world {wInfo = ["thinking..", ""], wPos = Just newPos}
                        let (aiReplyPosM, scoreM, status') = AI.bestDeepEval newPos depth
                        maybe
                            ( do
                                Printer.render world {wInfo = ["Game over: ", showt status'], wPos = Just newPos}
                                exit
                            )
                            ( \responsePos -> do
                                record responsePos
                                if terminal status'
                                    then do
                                        Printer.render world {wInfo = ["Game over: ", showt status'], wPos = Just responsePos, wScore = scoreM}
                                        exit
                                    else do
                                        Printer.render world {wInfo = ["Your move", ""], wPos = Just responsePos, wScore = scoreM}
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
                , wInfo = ["", ""]
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
        Left s -> do
            Printer.render world {wInfo = [pack s, "Valid syntax examples: e2-e4, d7-d8Q"]}
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
exit = Printer.exitText "Thank you for playing"

flightRecorder :: FilePath -> Position -> App ()
flightRecorder file pos = liftIO $ do
    let content = renderPgn pos
    TIO.writeFile ("pgn/" <> file) content