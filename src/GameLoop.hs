module GameLoop where

import AI (bestDeepEval)
import AppContext (App, AppContext (blackDepth, perspective, whiteDepth, startFrom), World (..))
import Chess (Status (BlackToPlay, WhiteToPlay), determineStatus, playIfLegal, positionTree)
import Evaluation (terminal)
import Move (parsedMove, playMove)
import PGN
import Position (
    Color (Black, White),
    Position,
    toPlay,
    startPosition,
 )
import qualified Printer

import Data.Text (pack)
import qualified Data.Text.IO as TIO
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Numeric (showFFloat)
import Relude
import qualified Data.Text as T

start :: Text -> App ()
start "1" = do
    time <- liftIO getCurrentTime
    let timeText = T.pack $ iso8601Show time
    Printer.clearTopScreen
    Printer.infoTexts ["Examples of moves are e2-e4 O-O-O d7-d8Q", ""]
    pos <- asks startFrom
    gameLoopHH timeText $ fromMaybe startPosition pos
start "2" = do
    time <- liftIO getCurrentTime
    let timeText = T.pack $ iso8601Show time
    perspective' <- asks perspective
    pos <- asks startFrom
    Printer.clearTopScreen
    case perspective' of
        White -> do
            depth <- asks blackDepth
            gameLoopHM timeText (fromMaybe startPosition pos) depth
        Black -> do
            depth <- asks whiteDepth
            let (Just opening, _, _) = AI.bestDeepEval (fromMaybe startPosition pos) depth
            gameLoopHM timeText opening depth
start "3" = do
    time <- liftIO getCurrentTime
    let timeText = T.pack $ iso8601Show time
    Printer.clearTopScreen
    wdepth <- asks whiteDepth
    bdepth <- asks blackDepth
    pos <- asks startFrom
    gameLoopMM timeText (fromMaybe startPosition pos) wdepth bdepth
start _ = exit

gameLoopHM :: Text -> Position -> Int -> App ()
gameLoopHM timeText pos depth = do
    let record = flightRecorder timeText "human" ("machine-" <> show depth)
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
                gameLoopHM timeText pos depth
            Right humanMove -> do
                case playIfLegal humanMove pos of
                    Left s -> do
                        Printer.render world {wInfo = [showt humanMove <> " not playable: " <> pack s, "example of syntax: e2-e4 h7-h8Q"]}
                        gameLoopHM timeText pos depth
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
                                        gameLoopHM timeText responsePos depth
                            )
                            aiReplyPosM

prettyScore :: Maybe Float -> String
prettyScore Nothing = ""
prettyScore (Just s) = formatFloatN s
  where
    formatFloatN floatNum = showFFloat (Just 2) floatNum ""

gameLoopMM :: Text -> Position -> Int -> Int -> App ()
gameLoopMM timeText pos whiteDepth blackDepth = do
    let record = flightRecorder timeText ("machine-" <> show whiteDepth) ("machine-" <> show blackDepth)
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
            gameLoopMM timeText responsePos whiteDepth blackDepth
        )
        pos'

gameLoopHH :: Text -> Position -> App ()
gameLoopHH timeText pos = do
    let record = flightRecorder timeText "human" "human"
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
            gameLoopHH timeText pos
        Right pos' ->
            let newStatus = determineStatus pos' (positionTree pos')
             in if newStatus == WhiteToPlay || newStatus == BlackToPlay
                    then
                        if l == ""
                            then exit
                            else gameLoopHH timeText pos'
                    else do
                        Printer.render world {wInfo = ["Game over: ", showt newStatus]}
                        flightRecorder timeText "human" "human" pos'
                        exit

showt :: Show a => a -> Text
showt = pack . show

exit :: App ()
exit = Printer.exitText "Thank you for playing"

flightRecorder :: Text -> Text -> Text -> Position -> App ()
flightRecorder timeText whiteName blackName pos = liftIO $ do
    let file = T.unpack $ whiteName <> "-" <> blackName <> "-" <> timeText <> ".pgn"
    let content = renderPgn timeText whiteName blackName pos
    TIO.writeFile ("pgn/" <> file) content