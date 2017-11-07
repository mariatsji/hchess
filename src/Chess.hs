module Chess(board, Color(..), Piece(..), Square, Position, startPosition, movePiece,
whitePawnMovesNaive, blackPawnMovesNaive, whiteKnightMovesNaive, blackKnightMovesNaive) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple

data Color = White | Black deriving (Eq, Ord, Show)
data Piece = Pawn Color | Knight Color | Bishop Color | Rook Color | Queen Color | King Color deriving (Eq, Ord, Show)

type Square = (Char, Int)
type Position = [(Square, Maybe Piece)]

board :: [Square]
board = fmap swap $ (,) <$> [1..8] <*> ['a'..'h']

row = snd . fst
col = fst . fst

unique :: Eq a => [a] -> [a]
unique = foldl (\a c -> if c `elem` a then a else c : a) []

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
removePieceAt pos square = fmap (\t -> if fst t == square then (fst t, Nothing) else t) pos

replacePieceAt :: Position -> Square -> Piece -> Position
replacePieceAt pos square piece = fmap (\t -> if fst t == square then (fst t, Just piece) else t) pos

pieceAt :: Position -> Square -> Maybe Piece
pieceAt pos square = find (\t -> fst t == square) pos >>= snd

findAll :: Position -> Piece -> [(Square, Piece)]
findAll pos piece = let allPieces = filter (\t -> snd t == Just piece) pos
    in fmap (\t -> (fst t, fromJust (snd t))) allPieces

stripOutsideBoard :: [Position] -> [Position]
stripOutsideBoard = filter (\p -> anyRowOutside p || anyColOutside p)
    where anyRowOutside = any (\sp -> row sp > 8 || row sp < 1)
          anyColOutside = any (\sp -> col sp > 'h' || col sp < 'a')


-- naive inside board
whiteMoves :: Position -> [Position]
whiteMoves pos = stripOutsideBoard $
    whitePawnMovesNaive pos ++
    whiteKnightMovesNaive pos ++ [] -- todo append more

whitePawnMovesNaive :: Position -> [Position]
whitePawnMovesNaive pos = whitePawnMovesNaive' pos $ findAll pos (Pawn White)

blackPawnMovesNaive :: Position -> [Position]
blackPawnMovesNaive pos = blackPawnMovesNaive' pos $ findAll pos (Pawn Black)

whitePawnMovesNaive' :: Position -> [(Square, Piece)] -> [Position]
whitePawnMovesNaive' pos pawns = pawns >>= whiteSinglePawnMoveNaive pos

blackPawnMovesNaive' :: Position -> [(Square, Piece)] -> [Position]
blackPawnMovesNaive' pos pawns = pawns >>= blackSinglePawnMoveNaive pos

whiteSinglePawnMoveNaive :: Position -> (Square, Piece) -> [Position]
whiteSinglePawnMoveNaive pos sp =
    movePiece pos (fst sp) (squareTo (fst sp) 1 0) :
    [movePiece pos (fst sp) (fmap (+2) (fst sp)) | row sp == 2]

blackSinglePawnMoveNaive :: Position -> (Square, Piece) -> [Position]
blackSinglePawnMoveNaive pos sp =
     movePiece pos (fst sp) (squareTo (fst sp) (-1) 0) :
     [movePiece pos (fst sp) (fmap (+(-2)) (fst sp)) | row sp == 7]

-- knights
whiteKnightMovesNaive :: Position -> [Position]
whiteKnightMovesNaive pos = knightMovesNaive' pos $ findAll pos (Knight White)

knightMovesNaive' :: Position -> [(Square, Piece)] -> [Position]
knightMovesNaive' pos knights = knights >>= knightSingleMoveNaive pos

knightSingleMoveNaive :: Position -> (Square, Piece) -> [Position]
knightSingleMoveNaive pos sp = fmap (movePiece pos (fst sp)) (toSquaresKnight (fst sp))

blackKnightMovesNaive :: Position -> [Position]
blackKnightMovesNaive pos = knightMovesNaive' pos $ findAll pos (Knight Black)


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

-- bishops
whiteBishopMovesNaive :: Position -> [Position]
whiteBishopMovesNaive pos = bishopMovesNaive' pos $ findAll pos (Bishop White)

blackBishopMovesNaive :: Position -> [Position]
blackBishopMovesNaive pos = bishopMovesNaive' pos $ findAll pos (Bishop Black)

bishopMovesNaive' :: Position -> [(Square, Piece)] -> [Position]
bishopMovesNaive' pos bishops = bishops >>= bishopSingleMoveNaive pos

bishopSingleMoveNaive :: Position -> (Square, Piece) -> [Position]
bishopSingleMoveNaive pos sp = fmap (movePiece pos (fst sp)) (toSquaresBishop (fst sp))

toSquaresBishop :: Square -> [Square]
toSquaresBishop s = unique [squareTo s a b |  a <- [-7..7], b <- [-7..7], abs a == abs b, (a,b) /= (0,0)]

-- rooks
whiteRookMovesNaive :: Position -> [Position]
whiteRookMovesNaive pos = rookMovesNaive pos $ findAll pos (Rook White)

blackRookMovesNaive :: Position -> [Position]
blackRookMovesNaive pos = rookMovesNaive pos $ findAll pos (Rook Black)

rookMovesNaive :: Position -> [(Square, Piece)] -> [Position]
rookMovesNaive pos rooks = rooks >>= rookSingleMoveNaive' pos

rookSingleMoveNaive' :: Position -> (Square, Piece) -> [Position]
rookSingleMoveNaive' pos sp = fmap (movePiece pos (fst sp)) (toSquaresRook (fst sp))

toSquaresRook :: Square -> [Square]
toSquaresRook s = unique [squareTo s a b |  a <- [-7..7], b <- [-7..7], a == 0 || b == 0, (a,b) /= (0,0)]

-- queens