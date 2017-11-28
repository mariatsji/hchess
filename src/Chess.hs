module Chess(board, Color(..), Piece(..), Square,
Position, GameHistory, startPosition, movePiece, whitePieces, blackPieces,
emptyBoard, replacePieceAt, positionTree, positionTreeIgnoreCheck,
canGoThere, finalDestinationNotOccupiedBySelf, points, points',
to, toSquaresPawn, pieceAt, toPlay, whiteToPlay, color, isInCheck,
anyPosWithoutKing, isCheckMate, isPatt, succ', promote, promoteBindFriendly) where

import Control.Arrow
import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple

data Color = White | Black deriving (Eq, Ord, Enum, Show)
data Piece = Pawn Color | Knight Color | Bishop Color | Rook Color | Queen Color | King Color deriving (Eq, Ord, Show)

type Square = (Char, Int)
type Position = [(Square, Maybe Piece)]
type GameHistory = [Position]

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

emptyBoard :: Position
emptyBoard = zip board (repeat Nothing)

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

whiteToPlay :: GameHistory -> Bool
whiteToPlay = odd . length

toPlay :: GameHistory -> Color
toPlay pos = if whiteToPlay pos then White else Black

positionTree :: GameHistory -> [Position]
positionTree gh = fmap head $ filter (\p -> not $ isInCheck (head p) (toPlay gh)) $ potentialGHs gh
  where potentialGHs gh' = (: gh') <$> positionTreeIgnoreCheck gh'

positionTreeIgnoreCheck :: GameHistory -> [Position] -- we know whos turn it is
positionTreeIgnoreCheck pos
    | whiteToPlay pos = whitePieces (head pos) >>= (\(s,p) -> positionsPrPiece (head pos) (s,p)) >>= (promoteBindFriendly White)
    | otherwise = blackPieces (head pos) >>= (\(s,p) -> positionsPrPiece (head pos) (s,p)) >>= (promoteBindFriendly Black)

positionTreeIgnoreCheck' :: Position -> Color -> [Position]
positionTreeIgnoreCheck' pos White = whitePieces pos >>= (positionsPrPiece pos)
positionTreeIgnoreCheck' pos Black = blackPieces pos >>= (positionsPrPiece pos)

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

-- promotions :: promote one position
prom :: Color -> Piece -> (Square, Maybe Piece) -> (Square, Maybe Piece)
prom White p (s, mp) = if snd s == 8 && mp == Just (Pawn White) then (s, Just p) else (s, mp)
prom Black p (s, mp) = if snd s == 1 && mp == Just (Pawn Black) then (s, Just p) else (s, mp)

-- promote one position
promoteTo :: Color -> Position -> Piece -> Position
promoteTo c pos p = fmap (prom c p) pos

-- promote one position to [] or all four positions
maybePromote :: Color -> Position -> Piece -> [Position]
maybePromote c pos p = if canPromote c pos p then [promoteTo c pos p] else []
  where canPromote c' pos' p' = promoteTo c' pos' p' /= pos'

promote :: Color -> Position -> [Position]
promote c@White pos = maybePromote c pos (Queen White) ++ maybePromote c pos (Rook White) ++ maybePromote c pos (Bishop White) ++ maybePromote c pos (Knight White)
promote c@Black pos = maybePromote c pos (Queen Black) ++ maybePromote c pos (Rook Black) ++ maybePromote c pos (Bishop Black) ++ maybePromote c pos (Knight Black)

-- same pos or all four
promoteBindFriendly :: Color -> Position -> [Position]
promoteBindFriendly c pos = if promote c pos /= [pos] && promote c pos /= [] then promote c pos else [pos]

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

-- castles

insideBoard :: Square -> Bool
insideBoard s = snd s >= 1 && snd s <= 8 && fst s >= 'a' && fst s <= 'h'

isInCheck :: Position -> Color -> Bool
isInCheck pos color = anyPosWithoutKing color (positionTreeIgnoreCheck' pos (succ' color))

isCheckMate :: GameHistory -> Bool
isCheckMate gh  = isInCheck (head gh) (toPlay gh) && null (positionTree gh)

isPatt :: GameHistory -> Bool
isPatt gh = not (isInCheck (head gh) (toPlay gh)) && null (positionTree gh)

anyPosWithoutKing :: Color -> [Position] -> Bool
anyPosWithoutKing col pos = not $ allHasKing col pos

allHasKing :: Color -> [Position] -> Bool
allHasKing White poses = all (any (\ (s, p) -> p == King White) . whitePieces) poses
allHasKing Black poses = all (any (\ (s, p) -> p == King Black) . blackPieces) poses