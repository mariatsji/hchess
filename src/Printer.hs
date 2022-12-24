module Printer (pretty, prettyE, render, infoTexts, line, clearTopScreen) where

import AppContext (App, AppContext (analysis), World (..))
import Board (diff)
import Chess (pieceAt)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Evaluation
import GHC.Exts
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
    let highlighted = unHash . fst <$> if null $ gamehistory pos then [] else m pos `diff` head (gamehistory pos)

    mapM_
        ( \(Square c r) -> do
            let mP = pieceAt pos (Square c r)
            if Square c r `elem` highlighted
                then ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Blue]
                else
                    if even (c + r)
                        then ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black]
                        else ANSI.setSGR [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.White]

            if c == 8
                then UP.putStrLn $ " " <> UF.fromString (prettyPiece (Square c r, mP)) <> " "
                else UP.putStr $ " " <> UF.fromString (prettyPiece (Square c r, mP)) <> " "
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

pretty :: Position -> IO ()
pretty pos = do
    mapM_ UP.putStrLn $ prettyRow <$> rowify pos

prettyE :: Evaluated -> IO ()
prettyE (Evaluated gh score status) = do
    pretty gh
    putStrLn $ "score : " ++ show score
    putStrLn $ "status : " ++ show status

rowify :: Position -> [[(Square, Maybe Piece)]]
rowify pos = reverse $ sortBy colSort <$> groupWith (\(Square _ r, _) -> r) (toList'' (m pos))
  where
    colSort :: (Square, Maybe Piece) -> (Square, Maybe Piece) -> Ordering
    colSort (s1, _) (s2, _) = compare s1 s2

prettyRow :: [(Square, Maybe Piece)] -> UF.ByteString
prettyRow row =
    UF.fromString $ foldl1 (\a s -> a ++ " " ++ s) $ fmap prettyPiece row

prettyPiece :: (Square, Maybe Piece) -> String
prettyPiece (_, Nothing) = " "
prettyPiece (_, Just (Pawn White)) = "♙"
prettyPiece (_, Just (Knight White)) = "♘"
prettyPiece (_, Just (Bishop White)) = "♗"
prettyPiece (_, Just (Rook White)) = "♖"
prettyPiece (_, Just (Queen White)) = "♕"
prettyPiece (_, Just (King White)) = "♔"
prettyPiece (_, Just (Pawn Black)) = "♟"
prettyPiece (_, Just (Knight Black)) = "♞"
prettyPiece (_, Just (Bishop Black)) = "♝"
prettyPiece (_, Just (Rook Black)) = "♜"
prettyPiece (_, Just (Queen Black)) = "♛"
prettyPiece (_, Just (King Black)) = "♚"
