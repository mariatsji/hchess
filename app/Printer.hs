module Printer (
pretty
) where

import Chess
import Data.List
import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF

pretty :: Position -> IO ()
pretty pos = do
    mapM UP.putStr $ fmap (UF.fromString . prettyPrint) pos
    return ()

prettyPrint :: (Square, Maybe Piece) -> String
prettyPrint (s, Nothing) = " "
prettyPrint (_, Just (Pawn White)) = "♙"
prettyPrint (_, Just (Knight White)) = "♘"
prettyPrint (_, Just (Bishop White)) = "♗"
prettyPrint (_, Just (Rook White)) = "♖"
prettyPrint (_, Just (Queen White)) = "♕"
prettyPrint (_, Just (King White)) = "♔"
prettyPrint (_, Just (Pawn Black)) = "♟"
prettyPrint (_, Just (Knight Black)) = "♞"
prettyPrint (_, Just (Bishop Black)) = "♝"
prettyPrint (_, Just (Rook Black)) = "♜"
prettyPrint (_, Just (Queen Black)) = "♛"
prettyPrint (_, Just (King Black)) = "♚"