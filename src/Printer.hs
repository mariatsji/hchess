module Printer where

import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF
import Data.List
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Evaluation
import GHC.Exts
import Position
import qualified System.Console.ANSI as ANSI
import System.IO (stdout)
import GHC.IO.Handle (hFlush)

infoTexts :: [String] -> IO ()
infoTexts [s1, s2] = do
    ANSI.setCursorPosition infoLineX infoLineY
    ANSI.clearLine
    putStr s1
    ANSI.setCursorPosition (infoLineX + 1) infoLineY
    ANSI.clearLine
    putStr s2
infoTexts _ = pure ()

infoLineX :: Int
infoLineX = 20

infoLineY :: Int
infoLineY = 0

prettyANSI :: Position -> IO ()
prettyANSI pos = do
    ANSI.setCursorPosition 5 0
    mapM_ UP.putStrLn $ prettyRow <$> rowify pos

line :: IO Text
line = do
    ANSI.setCursorPosition 22 0
    ANSI.clearLine
    hFlush stdout
    TIO.getLine

pretty :: Position -> IO ()
pretty pos = do
    mapM_ UP.putStrLn $ prettyRow <$> rowify pos
    putStrLn "-"
    return ()

prettyE :: Evaluated -> IO ()
prettyE (Evaluated gh score status) = do
    pretty gh
    putStrLn $ "score : " ++ show score
    putStrLn $ "status : " ++ show status

prettyEs :: [Evaluated] -> IO ()
prettyEs = mapM_ prettyE

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
