module Printer (render, infoTexts, line, clearTopScreen, exitText) where

import AppContext (App, AppContext (analysis, perspective), World (..), style)
import Board (diff)
import Chess (captures, pieceAt)
import Position
import Style

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (hFlush)
import Numeric (showFFloat)
import Relude
import qualified System.Console.ANSI as ANSI
import System.IO (stdout)

render :: World -> App ()
render World {..} = do
    title wTitle -- line 1
    maybe (pure ()) prettyANSI wPos -- 5 to 13
    renderScore wScore -- 14
    infoTexts wInfo -- 15 and 16
    maybe (pure ()) renderCaptures wPos

renderCaptures :: Position -> App ()
renderCaptures pos = do
    liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.White]
    perspective <- asks perspective
    style <- asks style
    let whitesCaptures = captures White pos
        blacksCaptures = captures Black pos
    liftIO $
        if perspective == White
            then ANSI.setCursorPosition 5 26
            else ANSI.setCursorPosition 12 26
    traverse_
        ( \p -> liftIO do
            let (pieceString, style') = prettyPiece style (Just p)
            ANSI.setSGR style'
            UP.putStr $ UF.fromString pieceString
        )
        blacksCaptures
    liftIO $
        if perspective == White
            then ANSI.setCursorPosition 12 26
            else ANSI.setCursorPosition 5 26
    traverse_
        ( \p -> liftIO do
            let (pieceString, style') = prettyPiece style (Just p)
            ANSI.setSGR style'
            UP.putStr $ UF.fromString pieceString
        )
        whitesCaptures
    liftIO $ ANSI.setSGR (fonts style)

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

exitText :: Text -> App ()
exitText t = do
    liftIO $ do
        ANSI.setCursorPosition 22 0
        putStrLn $ T.unpack t

clearTopScreen :: App ()
clearTopScreen = do
    liftIO $ do
        ANSI.setCursorPosition 4 0
        ANSI.clearFromCursorToScreenBeginning

clearInfo :: App ()
clearInfo = liftIO $ do
    ANSI.setCursorPosition 22 0
    ANSI.clearLine
    ANSI.setCursorPosition infoLineX infoLineY
    ANSI.clearLine
    ANSI.setCursorPosition (infoLineX + 1) infoLineY
    ANSI.clearLine

infoLineX :: Int
infoLineX = 15

infoLineY :: Int
infoLineY = 0

prettyANSI :: Position -> App ()
prettyANSI pos = do
    perspective' <- asks perspective
    style <- asks style
    liftIO $ do
        ANSI.setCursorPosition 5 0
        let highlighted =
                unHash <$.>
                    if null $ gamehistory pos then [] else head (gamehistory pos) `diff` m pos
        ANSI.setSGR (fonts style)
        let rows = case perspective' of
                White -> flip Square <$> [8 :: Int, 7 .. 1] <*> [1 :: Int .. 8] -- a8, b8 ..
                Black -> flip Square <$> [1 :: Int .. 8] <*> [1 :: Int .. 8] -- a1, b1 ..
        mapM_
            ( \(Square c r) -> do
                let mP = pieceAt pos (Square c r)
                if Square c r `elem` (fst <$> highlighted)
                    then ANSI.setSGR (highlightedSquare style)
                    else
                        if even (c + r)
                            then ANSI.setSGR (lightSquare style)
                            else ANSI.setSGR (darkSquare style)

                let (pieceString, style') = prettyPiece style mP
                ANSI.setSGR style'
                ( if c == 8
                        then UP.putStrLn
                        else UP.putStr
                    )
                    $ " " <> UF.fromString pieceString <> " "

                ANSI.setSGR (fonts style)
            )
            rows
        ANSI.setSGR (fonts style)

line :: App Text
line = do
    liftIO $ do
        ANSI.setCursorPosition 22 0
        ANSI.clearLine
        hFlush stdout
        TIO.getLine

prettyPiece :: Style -> Maybe Piece -> (String, [ANSI.SGR])
prettyPiece Style {..} Nothing = (" ", emptySquare)
prettyPiece Style {..} (Just (Pawn White)) = ("♟", whitePiece)
prettyPiece Style {..} (Just (Knight White)) = ("♞", whitePiece)
prettyPiece Style {..} (Just (Bishop White)) = ("♝", whitePiece)
prettyPiece Style {..} (Just (Rook White)) = ("♜", whitePiece)
prettyPiece Style {..} (Just (Queen White)) = ("♛", whitePiece)
prettyPiece Style {..} (Just (King White)) = ("♚", whitePiece)
prettyPiece Style {..} (Just (Pawn Black)) = ("♟", blackPiece)
prettyPiece Style {..} (Just (Knight Black)) = ("♞", blackPiece)
prettyPiece Style {..} (Just (Bishop Black)) = ("♝", blackPiece)
prettyPiece Style {..} (Just (Rook Black)) = ("♜", blackPiece)
prettyPiece Style {..} (Just (Queen Black)) = ("♛", blackPiece)
prettyPiece Style {..} (Just (King Black)) = ("♚", blackPiece)
