module Printer (render, infoTexts, line, clearTopScreen) where

import AppContext (App, AppContext (analysis), World (..))
import Board (diff)
import Chess (pieceAt)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (hFlush)
import Numeric (showFFloat)
import Position
import qualified System.Console.ANSI as ANSI
import System.IO (stdout)

render :: World -> App ()
render World {..} = do
    title wTitle -- line 1
    maybe (pure ()) prettyANSI wPos -- 5 to 13
    renderScore wScore -- 14
    infoTexts wInfo -- 15 and 16

renderScore :: Maybe Float -> App ()
renderScore Nothing = pure ()
renderScore (Just s) = do
    showAnalysis <- asks analysis
    liftIO $ do
        ANSI.setCursorPosition 14 0
        when showAnalysis $ putStrLn $ formatFloatN s
  where
    formatFloatN floatNum = showFFloat (Just 2) floatNum ""

title :: Text -> App ()
title t = do
    liftIO $ do
        ANSI.setCursorPosition 1 0
        ANSI.clearLine
        putStrLn $ T.unpack t

infoTexts :: [Text] -> App ()
infoTexts [s1, s2] = do
    clearInfo
    liftIO $ do
        ANSI.setCursorPosition infoLineX infoLineY
        putStrLn $ T.unpack s1
        ANSI.setCursorPosition (infoLineX + 1) infoLineY
        putStrLn $ T.unpack s2
infoTexts _ = pure ()

clearTopScreen :: App ()
clearTopScreen = do
    liftIO $ do
        ANSI.setCursorPosition 4 0
        ANSI.clearFromCursorToScreenBeginning

clearInfo :: App ()
clearInfo = liftIO $ do
    ANSI.setCursorPosition infoLineX infoLineY
    ANSI.clearLine
    ANSI.setCursorPosition (infoLineX + 1) infoLineY
    ANSI.clearLine

infoLineX :: Int
infoLineX = 15

infoLineY :: Int
infoLineY = 0

prettyANSI :: Position -> App ()
prettyANSI pos = liftIO $ do
    ANSI.setCursorPosition 5 0
    let highlighted =
            filter (\(_, mP) -> isJust mP) $
                unHash <$.>
                    if null $ gamehistory pos then [] else head (gamehistory pos) `diff` m pos
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
    mapM_
        ( \(Square c r) -> do
            let mP = pieceAt pos (Square c r)
            if Square c r `elem` (fst <$> highlighted)
                then ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Cyan]
                else
                    if even (c + r)
                        then ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Blue]
                        else ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.White]

            let (pieceString, style) = prettyPiece (Square c r, mP)
            ANSI.setSGR style
            ( if c == 8
                    then UP.putStrLn
                    else UP.putStr
                )
                $ " " <> UF.fromString pieceString <> " "

            ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.White]
        )
        (flip Square <$> [8 :: Int, 7 .. 1] <*> [1 :: Int .. 8]) -- a8, b8 ..

line :: App Text
line = do
    liftIO $ do
        ANSI.setCursorPosition 22 0
        ANSI.clearLine
        hFlush stdout
        TIO.getLine

prettyPiece :: (Square, Maybe Piece) -> (String, [ANSI.SGR])
prettyPiece (_, Nothing) = (" ", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black])
prettyPiece (_, Just (Pawn White)) = ("♟", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White])
prettyPiece (_, Just (Knight White)) = ("♞", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White])
prettyPiece (_, Just (Bishop White)) = ("♝", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White])
prettyPiece (_, Just (Rook White)) = ("♜", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White])
prettyPiece (_, Just (Queen White)) = ("♛", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White])
prettyPiece (_, Just (King White)) = ("♚", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White])
prettyPiece (_, Just (Pawn Black)) = ("♟", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black])
prettyPiece (_, Just (Knight Black)) = ("♞", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black])
prettyPiece (_, Just (Bishop Black)) = ("♝", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black])
prettyPiece (_, Just (Rook Black)) = ("♜", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black])
prettyPiece (_, Just (Queen Black)) = ("♛", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black])
prettyPiece (_, Just (King Black)) = ("♚", [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black])
