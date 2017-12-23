module Printer (
pretty,
prettyGH,
prettyE
) where

import Chess
import Data.Char
import Evaluation
import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF
import GHC.Exts

pretty :: Position -> IO ()
pretty pos = do
    mapM_ UP.putStrLn $ fmap prettyRow $ rowify pos
    putStrLn "-"
    return ()

prettyE :: Evaluated -> IO ()
prettyE (gh, score, status) = do
  prettyGH gh
  putStrLn $ "score : " ++ show score
  putStrLn $ "status : " ++ show status

prettyGH :: GameHistory -> IO ()
prettyGH gh = mapM_ pretty (reverse gh)

rowify :: Position -> [[(Square, Maybe Piece)]]
rowify = groupWith ((* (-1)) . snd . fst)

prettyRow :: [(Square, Maybe Piece)] -> UF.ByteString
prettyRow row = UF.fromString $ foldl1 (\a s -> a ++ " " ++ s) $ fmap prettyPiece row

prettyPiece :: (Square, Maybe Piece) -> String
prettyPiece ((c,r), Nothing) = if even (ord c + r) then "◽" else " "
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
