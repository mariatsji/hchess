module Chess(board, Color(..), Piece(..), Square, Position, startPosition, movePiece) where

import Data.List
import Data.Tuple

data Color = White | Black deriving (Eq, Ord, Show)
data Piece = Pawn Color | Knight Color | Bishop Color | Rook Color | Queen Color | King Color deriving (Eq, Ord, Show)

type Square = (Char, Int)
type Position = [(Square, Maybe Piece)]

board :: [Square]
board = fmap swap $ ((,)) <$> [1..8] <*> ['a'..'h']

startPosition :: Position
startPosition = zip board ([Just $ Rook White, Just $ Knight White, Just $ Bishop White, Just $ Queen White, Just $ King White, Just $ Bishop White, Just $ Knight White, Just $ Rook White]
              ++ (take 8 $ repeat (Just $ Pawn White))
              ++ (take 32 $ repeat Nothing)
              ++ (take 8 $ repeat (Just $ Pawn Black))
              ++           [Just $ Rook Black, Just $ Knight Black, Just $ Bishop Black, Just $ Queen Black, Just $ King Black, Just $ Bishop Black, Just $ Knight Black, Just $ Rook Black])


movePiece :: Position -> Square -> Square -> Position
movePiece pos from to = case (pieceAt pos from) of (Just piece) -> replacePieceAt (removePieceAt pos from) to piece
                                                   Nothing -> pos

removePieceAt :: Position -> Square -> Position
removePieceAt pos square = fmap (\t -> if (fst t == square) then (fst t, Nothing) else t) pos

replacePieceAt :: Position -> Square -> Piece -> Position
replacePieceAt pos square piece = fmap (\t -> if (fst t == square) then (fst t, Just piece) else t) pos

pieceAt :: Position -> Square -> Maybe Piece
pieceAt pos square = find (\t -> fst t == square) pos >>= snd