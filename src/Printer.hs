module Printer (pretty, prettyE, render, infoTexts, line) where

import AppContext (App, AppContext (analysis), World (..))
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
    title wTitle
    maybe (pure ()) prettyANSI wPos
    renderScore wScore
    infoTexts wInfo

renderScore :: Maybe Float -> App ()
renderScore Nothing = pure ()
renderScore (Just s) = do
    showAnalysis <- asks analysis
    liftIO $ do
        ANSI.setCursorPosition 16 0
        when showAnalysis $ putStrLn $ formatFloatN s
  where
    formatFloatN floatNum = showFFloat (Just 2) floatNum ""

title :: Text -> App ()
title t = do
    liftIO $ do
        ANSI.setCursorPosition 1 0
        ANSI.clearFromCursorToScreenBeginning
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

clearInfo :: App ()
clearInfo = liftIO $ do
    ANSI.setCursorPosition 5 0
    ANSI.clearFromCursorToScreenBeginning
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
    mapM_ UP.putStrLn $ prettyRow <$> rowify pos

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
rowify pos = reverse $ sortBy colSort <$> groupWith (\(Square _ r, _) -> r) (toList' (m pos))
  where
    colSort :: (Square, Maybe Piece) -> (Square, Maybe Piece) -> Ordering
    colSort (s1, _) (s2, _) = compare s1 s2

prettyRow :: [(Square, Maybe Piece)] -> UF.ByteString
prettyRow row =
    UF.fromString $ foldl1 (\a s -> a ++ " " ++ s) $ fmap prettyPiece row

prettyPiece :: (Square, Maybe Piece) -> String
prettyPiece (Square c r, Nothing) =
    if even (c + r)
        then "▯"
        else " "
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
