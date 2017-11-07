module Printer (
pretty
) where

import Chess
import Data.List
import Data.Ord
import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF
import GHC.Exts

pretty :: Position -> IO ()
pretty pos = do
    mapM_ UP.putStrLn $ fmap prettyRow $ rowify $ sortP pos
    return ()

rowify :: Position -> [[(Square, Maybe Piece)]]
rowify = groupWith (snd . fst)

prettyRow :: [(Square, Maybe Piece)] -> UF.ByteString
prettyRow row = UF.fromString $ foldl1 (\a s -> a ++ " " ++ s) $ fmap prettyPiece row

prettyPiece :: (Square, Maybe Piece) -> String
prettyPiece (s, Nothing) = " "
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

sortP :: Position -> Position
sortP = sortBy compareS

compareS :: (Square, Maybe Piece) -> (Square, Maybe Piece) -> Ordering
compareS a b = if row a == row b
    then compare (col a) (col b)
    else compare (row a) (row b)
        where row = snd . fst
              col = fst . fst