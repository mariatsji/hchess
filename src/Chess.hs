module Chess(board, Color(..), Piece(..), Square, Position, startPosition, movePiece, whitePieces, blackPieces, positionTree) where

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

color :: Piece -> Color
color (Pawn color) = color
color (Knight color) = color
color (Bishop color) = color
color (Rook color) = color
color (Queen color) = color
color (King color) = color

row = snd . fst
col = fst . fst

unique :: Eq a => [a] -> [a]
unique = foldl (\a c -> if c `elem` a then a else c : a) []

squareTo :: Square -> Int -> Int -> Square
squareTo (c,r) cols rows = (chr (ord c + cols), r + rows)

startPosition :: Position
startPosition = zip board ([Just $ Rook White, Just $ Knight White, Just $ Bishop White, Just $ Queen White, Just $ King White, Just $ Bishop White, Just $ Knight White, Just $ Rook White]
              ++ replicate 8 (Just $ Pawn White)
              ++ replicate 32 Nothing
              ++ replicate 8 (Just $ Pawn Black)
              ++ [Just $ Rook Black, Just $ Knight Black, Just $ Bishop Black, Just $ Queen Black, Just $ King Black, Just $ Bishop Black, Just $ Knight Black, Just $ Rook Black])


movePiece :: Position -> Square -> Square -> Position
movePiece pos from to = case pieceAt pos from of (Just piece) -> replacePieceAt (removePieceAt pos from) to piece
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

whitePieces :: Position -> [(Square, Piece)]
whitePieces pos = (\t -> (fst t, fromJust (snd t))) <$> filter isWhite pos

isWhite :: (Square, Maybe Piece) -> Bool
isWhite (_, Nothing) = False
isWhite (_, Just p)
    | color p == White = True
    | otherwise = False

isBlack :: (Square, Maybe Piece) -> Bool
isBlack (_, Nothing) = False
isBlack (_, Just p)
    | color p == Black = True
    | otherwise = False

blackPieces :: Position -> [(Square, Piece)]
blackPieces pos = (\t -> (fst t, fromJust (snd t))) <$> filter isBlack pos

positionTree :: Position -> Color -> [Position]
positionTree pos colr = whitePieces pos >>= (\(s,p) -> positionsPrPiece pos (s,p))

positionsPrPiece :: Position -> (Square, Piece) -> [Position]
positionsPrPiece pos (s,p) = case p of (Pawn _) -> fmap (movePiece pos s) (toSquaresPawn (s, p))
                                       (Knight _) -> fmap (movePiece pos s) (toSquaresKnight s)
                                       (Bishop _) -> fmap (movePiece pos s) (toSquaresBishop s)
                                       (Rook _) -> fmap (movePiece pos s) (toSquaresRook s)
                                       (Queen _) -> fmap (movePiece pos s) (toSquaresQueen s)
                                       (King _) -> fmap (movePiece pos s) (toSquaresKing s)

-- pawns
toSquaresPawn :: (Square, Piece) -> [Square]
toSquaresPawn (s, p)
        | color p == White = filter insideBoard $
            if snd s == 2 then [squareTo s 2 0] else [] ++
                [squareTo s 1 0] ++
                [squareTo s 1 (-1)] ++
                [squareTo s 1 1]
        | otherwise = filter insideBoard $
            if snd s == 7 then [squareTo s (-2) 0] else [] ++
                [squareTo s (-1) 0] ++
                [squareTo s (-1) (-1)] ++
                [squareTo s (-1) 1]


-- knights
toSquaresKnight :: Square -> [Square]
toSquaresKnight s = filter insideBoard [
        squareTo s (-1) 2,
        squareTo s (-1) (-2),
        squareTo s 1 2,
        squareTo s 1 (-2),
        squareTo s 2 1,
        squareTo s 2 (-1),
        squareTo s (-2) 1,
        squareTo s (-2) (-1)]

-- bishops
toSquaresBishop :: Square -> [Square]
toSquaresBishop s = unique [squareTo s a b |  a <- [-7..7], b <- [-7..7], abs a == abs b, (a,b) /= (0,0), insideBoard $ squareTo s a b]

-- rooks
toSquaresRook :: Square -> [Square]
toSquaresRook s = unique [squareTo s a b |  a <- [-7..7], b <- [-7..7], a == 0 || b == 0, (a,b) /= (0,0), insideBoard $ squareTo s a b]

-- queens
toSquaresQueen :: Square -> [Square]
toSquaresQueen s = toSquaresBishop s `mappend` toSquaresRook s

-- kings
toSquaresKing :: Square -> [Square]
toSquaresKing s = [squareTo s a b | a <- [-1, 0, 1], b <- [-1, 0, 1], (a,b) /= (0,0), insideBoard $ squareTo s a b]

insideBoard :: Square -> Bool
insideBoard s = snd s >= 1 && snd s <= 8 && fst s >= 'a' && fst s <= 'h'