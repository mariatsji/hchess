module Chess(board, Color(..), Piece(..), Square,
Position, startPosition, movePiece, whitePieces, blackPieces,
positionTree, canGoThere, finalDestinationNotOccupiedBySelf, points, points', to,
toSquaresPawn, pieceAt, whiteToPlay, color) where

import Control.Arrow
import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple

data Color = White | Black deriving (Eq, Ord, Enum, Show)
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
movePiece pos from to =
    case pieceAt pos from of (Just piece) -> replacePieceAt (removePieceAt pos from) to piece
                             Nothing -> pos

points' :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -- all visited squares
points' (c1, r1) (c2, r2)
    | c1 == c2 || r1 == r2 = [(a,b) | a <- [min c1 c2 .. max c1 c2], b <- [min r1 r2..max r1 r2], (a,b) /= (c1,r1), (a,b) /= (c2,r2)]
    | otherwise = filter (\(a,b) -> (a,b) /= (c1, r1) && (a,b) /= (c2, r2)) $ zip (c1 `to` c2) (r1 `to` r2)

to :: Int -> Int -> [Int]
to a b
    | a == b = [a]
    | a > b = a : to (a - 1) b
    | otherwise = a : to (a + 1) b

points :: Square -> Square -> [Square]
points (c1, r1) (c2, r2) = first chr <$> points' (ord c1, r1) (ord c2, r2)

canGoThere :: Position -> Square -> Square -> Bool
canGoThere pos from to = all isNothing (fmap (pieceAt pos) (points from to)) && finalDestinationNotOccupiedBySelf pos from to

finalDestinationNotOccupiedBySelf :: Position -> Square -> Square -> Bool
finalDestinationNotOccupiedBySelf pos f t = fmap color (pieceAt pos t) /= fmap color (pieceAt pos f)

enemyAt :: Position -> Square -> Square -> Bool
enemyAt pos f t = fmap (succ' . color) (pieceAt pos t) == fmap color (pieceAt pos f)

succ' :: Color -> Color
succ' White = Black
succ' Black = White

vacantAt :: Position -> Square -> Square -> Bool
vacantAt pos f t = isNothing $ pieceAt pos t

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

whiteToPlay :: [Position] -> Bool
whiteToPlay = odd . length

positionTree :: [Position] -> [Position] -- we know whos turn it is
positionTree pos
    | whiteToPlay pos = whitePieces (head pos) >>= (\(s,p) -> positionsPrPiece (head pos) (s,p))
    | otherwise = blackPieces (head pos) >>= (\(s,p) -> positionsPrPiece (head pos) (s,p))

positionsPrPiece :: Position -> (Square, Piece) -> [Position] -- wedge canGoThere into here
positionsPrPiece pos (s,p) = case p of (Pawn _) -> fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresPawn pos (s, p))
                                       (Knight _) -> fmap (movePiece pos s) (filter (finalDestinationNotOccupiedBySelf pos s) $ toSquaresKnight s)
                                       (Bishop _) -> fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresBishop s)
                                       (Rook _) -> fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresRook s)
                                       (Queen _) -> fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresQueen s)
                                       (King _) -> fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresKing s)

-- pawns
toSquaresPawn :: Position -> (Square, Piece) -> [Square]
toSquaresPawn pos (s, p)
        | color p == White = filter insideBoard $
            [squareTo s 0 2 | snd s == 2, vacantAt pos s $ squareTo s 0 2] ++
            [squareTo s 0 1 | vacantAt pos s $ squareTo s 0 1] ++
            [squareTo s (-1) 1 | enemyAt pos s $ squareTo s (-1) 1] ++
            [squareTo s 1 1 | enemyAt pos s $ squareTo s 1 1]
        | otherwise = filter insideBoard $
            [squareTo s 0 (-2) | snd s == 7, vacantAt pos s $ squareTo s 0 (-2)] ++
            [squareTo s 0 (-1) | vacantAt pos s $ squareTo s 0 (-1)] ++
            [squareTo s (-1) (-1) | enemyAt pos s $ squareTo s (-1) (-1)] ++
            [squareTo s 1 (-1) | enemyAt pos s $ squareTo s 1 (-1)]


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