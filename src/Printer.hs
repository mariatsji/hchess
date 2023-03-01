module Printer (render, infoTexts, line, clearTopScreen, exitText) where

import AppContext (App, AppContext (analysis, perspective), World (..), style)
import Board (diff)
import Chess (captures, pieceAt)
import Position
import Style

import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Numeric (showFFloat)
import Relude
import qualified System.Console.ANSI as ANSI

render :: World -> App ()
render World {..} = do
    title wTitle -- line 1
    maybe (pure ()) prettyANSI wPos -- 3 to 11
    renderScore wScore -- 12
    infoTexts wInfo -- 13 and 14
    maybe (pure ()) renderCaptures wPos
    -- input at line 18

renderCaptures :: Position -> App ()
renderCaptures pos = do
    liftIO $ ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.White]
    perspective <- asks perspective
    style <- asks style
    let whitesCaptures = captures White pos
        blacksCaptures = captures Black pos
    liftIO $
        if perspective == White
            then ANSI.setCursorPosition 3 28
            else ANSI.setCursorPosition 10 28
    traverse_
        ( \p -> liftIO do
            let (pieceString, style') = prettyPiece style (Just p)
            ANSI.setSGR style'
            UP.putStr $ UF.fromString pieceString
        )
        blacksCaptures
    liftIO $
        if perspective == White
            then ANSI.setCursorPosition 10 28
            else ANSI.setCursorPosition 3 28
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
        ANSI.setCursorPosition 12 0
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
        ANSI.setCursorPosition 18 0
        putStrLn $ T.unpack t

clearTopScreen :: App ()
clearTopScreen = do
    liftIO $ do
        ANSI.setCursorPosition 2 0
        ANSI.clearFromCursorToScreenBeginning

clearInfo :: App ()
clearInfo = liftIO $ do
    ANSI.setCursorPosition 18 0
    ANSI.clearLine
    ANSI.setCursorPosition infoLineX infoLineY
    ANSI.clearLine
    ANSI.setCursorPosition (infoLineX + 1) infoLineY
    ANSI.clearLine

infoLineX :: Int
infoLineX = 13

infoLineY :: Int
infoLineY = 0

prettyANSI :: Position -> App ()
prettyANSI pos = do
    perspective' <- asks perspective
    style <- asks style
    liftIO $ do
        ANSI.setCursorPosition 3 0
        let highlighted = case gamehistory pos of
                pos1 : _ -> unHash <$.> (pos1 `diff` m pos)
                _ -> []
        ANSI.setSGR (fonts style)
        let rows = case perspective' of
                White -> flip Square <$> [8 :: Int, 7 .. 1] <*> [1 :: Int .. 8] -- a8, b8 ..
                Black -> flip Square <$> [1 :: Int .. 8] <*> [1 :: Int .. 8] -- a1, b1 ..
        mapM_
            ( \(Square c r) -> do
                when (c == 1) $ do
                    ANSI.setSGR (fonts style)
                    UP.putStr $ show r <> " "
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
        UP.putStrLn "   a  b  c  d  e  f  g  h "
        ANSI.setSGR (fonts style)

line :: App Text
line = do
    liftIO $ do
        ANSI.setCursorPosition 18 0
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
