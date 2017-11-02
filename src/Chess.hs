module Chess(board, Color(..), Piece(..), Square, Position, startPosition, movePiece, whitePawnMovesNaive, whiteKnightMovesNaive) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple

data Color = White | Black deriving (Eq, Ord, Show)
data Piece = Pawn Color | Knight Color | Bishop Color | Rook Color | Queen Color | King Color deriving (Eq, Ord, Show)

type Square = (Char, Int)
type Position = [(Square, Maybe Piece)]

board :: [Square]
board = fmap swap $ ((,)) <$> [1..8] <*> ['a'..'h']

row = (snd . fst)
col = (fst . fst)

squareTo :: Square -> Int -> Int -> Square
squareTo (c,r) cols rows = (chr (ord c + cols), r + rows)

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

findAll :: Position -> Piece -> [(Square, Piece)]
findAll pos piece = let allPieces = filter (\t -> snd t == Just piece) pos
    in fmap (\t -> (fst t, fromJust (snd t))) allPieces

stripOutsideBoard :: [Position] -> [Position]
stripOutsideBoard pos = filter (\p -> anyRowOutside p || anyColOutside p) pos
    where anyRowOutside = any (\sp -> (row sp) > 8 || (row sp) < 1)
          anyColOutside = any (\sp -> (col sp) > 'h' || (col sp) < 'a')


-- naive inside board
whiteMoves :: Position -> [Position]
whiteMoves pos = stripOutsideBoard $
    whitePawnMovesNaive pos ++
    whiteKnightMovesNaive pos ++ [] -- todo append more

whitePawnMovesNaive :: Position -> [Position]
whitePawnMovesNaive pos = whitePawnMovesNaive' pos $ findAll pos (Pawn White)

whitePawnMovesNaive' :: Position -> [(Square, Piece)] -> [Position]
whitePawnMovesNaive' pos pawns = pawns >>= (whiteSinglePawnMoveNaive pos)

whiteSinglePawnMoveNaive :: Position -> (Square, Piece) -> [Position]
whiteSinglePawnMoveNaive pos sp =
    movePiece pos (fst sp) (squareTo (fst sp) 1 0) :
    if ((row sp) == 2) then [movePiece pos (fst sp) (fmap (+2) (fst sp))] else []

-- knights
whiteKnightMovesNaive :: Position -> [Position]
whiteKnightMovesNaive pos = whiteKnightMovesNaive' pos $ findAll pos (Knight White)

whiteKnightMovesNaive' :: Position -> [(Square, Piece)] -> [Position]
whiteKnightMovesNaive' pos knights = knights >>= (whiteKnightSingleMoveNaive pos)

whiteKnightSingleMoveNaive :: Position -> (Square, Piece) -> [Position]
whiteKnightSingleMoveNaive pos sp = fmap (\s -> movePiece pos (fst sp) s) (toSquaresKnight (fst sp))

toSquaresKnight :: Square -> [Square]
toSquaresKnight s = [
        squareTo s (-1) 2,
        squareTo s (-1) (-2),
        squareTo s 1 2,
        squareTo s 1 (-2),
        squareTo s 2 1,
        squareTo s 2 (-1),
        squareTo s (-2) 1,
        squareTo s (-2) (-1)]



