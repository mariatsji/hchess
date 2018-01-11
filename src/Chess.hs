module Chess(board, Color(..), Piece(..), Square,
Position, GameHistory, Status(..), startPosition, movePiece, makeMoves, removePieceAt, whitePieces, blackPieces,
emptyBoard, replacePieceAt, positionTree, positionTree', positionTreeIgnoreCheck, enPassant,
canGoThere, finalDestinationNotOccupiedBySelf, points, points', eqPosition, positionsPrPiece,
to', toSquaresPawn, pieceAt, toPlay, whiteToPlay, colr, isInCheck,
anyPosWithoutKing, isCheckMate, isPatt, threefoldrepetition,
posrep, isDraw, succ', promote, promoteTo, promoteBindFriendly, castle, castleShort, castleLong, determineStatus) where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Tuple

data Color = White | Black deriving (Eq, Ord, Enum, Show)
data Piece = Pawn Color | Knight Color | Bishop Color | Rook Color | Queen Color | King Color deriving (Eq, Ord, Show)

type Square = (Char, Int)
type Position = Map.Map Square (Maybe Piece)
type GameHistory = [Position]

data Status = WhiteToPlay | BlackToPlay | Remis | WhiteIsMate | BlackIsMate deriving (Eq, Ord, Show)

board :: [Square]
board = fmap swap $ (,) <$> [1..8] <*> ['a'..'h']

colr :: Piece -> Color
colr (Pawn c) = c
colr (Knight c) = c
colr (Bishop c) = c
colr (Rook c) = c
colr (Queen c) = c
colr (King c) = c

king :: Color -> Piece
king White = King White
king Black = King Black

rook :: Color -> Piece
rook White = Rook White
rook Black = Rook Black

unique :: Eq a => [a] -> [a]
unique = foldl (\a c -> if c `elem` a then a else c : a) []

squareTo :: Square -> Int -> Int -> Square
squareTo (c,r) cols rows = (chr (ord c + cols), r + rows)

startPosition :: Position
startPosition = Map.fromList $
                 zip board ([Just $ Rook White, Just $ Knight White, Just $ Bishop White, Just $ Queen White, Just $ King White, Just $ Bishop White, Just $ Knight White, Just $ Rook White]
              ++ replicate 8 (Just $ Pawn White)
              ++ replicate 32 Nothing
              ++ replicate 8 (Just $ Pawn Black)
              ++ [Just $ Rook Black, Just $ Knight Black, Just $ Bishop Black, Just $ Queen Black, Just $ King Black, Just $ Bishop Black, Just $ Knight Black, Just $ Rook Black])

emptyBoard :: Position
emptyBoard = Map.fromList $ zip board (repeat Nothing)

movePiece :: Position -> Square -> Square -> Position
movePiece pos from to
  | pieceAt pos from == Just (Pawn White) && (fst from /= fst to) && vacantAt pos to = movePiece' (removePieceAt pos (fst to, snd to - 1)) from to
  | pieceAt pos from == Just (Pawn Black) && (fst from /= fst to) && vacantAt pos to = movePiece' (removePieceAt pos (fst to, snd to + 1)) from to
  | otherwise = movePiece' pos from to

movePiece' :: Position -> Square -> Square -> Position
movePiece' pos from to =
    case pieceAt pos from of (Just piece) -> replacePieceAt (removePieceAt pos from) to piece
                             Nothing -> pos

points' :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -- all visited squares
points' (c1, r1) (c2, r2)
    | c1 == c2 || r1 == r2 = [(a,b) | a <- [min c1 c2 .. max c1 c2], b <- [min r1 r2..max r1 r2], (a,b) /= (c1,r1), (a,b) /= (c2,r2)]
    | otherwise = filter (\(a,b) -> (a,b) /= (c1, r1) && (a,b) /= (c2, r2)) $ zip (c1 `to'` c2) (r1 `to'` r2)

to' :: Int -> Int -> [Int]
to' a b
    | a == b = [a]
    | a > b = a : to' (a - 1) b
    | otherwise = a : to' (a + 1) b

points :: Square -> Square -> [Square]
points (c1, r1) (c2, r2) = first chr <$> points' (ord c1, r1) (ord c2, r2)

canGoThere :: Position -> Square -> Square -> Bool
canGoThere pos from to = all isNothing (fmap (pieceAt pos) (points from to)) && finalDestinationNotOccupiedBySelf pos from to

finalDestinationNotOccupiedBySelf :: Position -> Square -> Square -> Bool
finalDestinationNotOccupiedBySelf pos f t = fmap colr (pieceAt pos t) /= fmap colr (pieceAt pos f)

enemyAt :: Position -> Square -> Square -> Bool
enemyAt pos f t = fmap (succ' . colr) (pieceAt pos t) == fmap colr (pieceAt pos f)

succ' :: Color -> Color
succ' White = Black
succ' Black = White

vacantAt :: Position -> Square -> Bool
vacantAt pos t = isNothing $ pieceAt pos t

removePieceAt :: Position -> Square -> Position
removePieceAt pos square = Map.adjust (\_ -> Nothing) square pos

replacePieceAt :: Position -> Square -> Piece -> Position
replacePieceAt pos square piece = Map.adjust (\_ -> Just piece) square pos

makeMoves :: GameHistory -> [(Square, Square)] -> GameHistory
makeMoves gh [] = gh
makeMoves gh (x:xs) = makeMoves (movePiece (head gh) (fst x) (snd x) : gh) xs

-- (a,1), (b,1) .. (h,8)
pieceAt :: Position -> Square -> Maybe Piece
pieceAt pos square = join $ pos Map.!? square

whitePieces :: Position -> [(Square, Piece)]
whitePieces pos = fmap (\t -> (fst t, fromJust (snd t))) $ filter isWhite $ Map.assocs pos

isWhite :: (Square, Maybe Piece) -> Bool
isWhite (_, Nothing) = False
isWhite (_, Just p)
    | colr p == White = True
    | otherwise = False

isBlack :: (Square, Maybe Piece) -> Bool
isBlack (_, Nothing) = False
isBlack (_, Just p)
    | colr p == Black = True
    | otherwise = False

blackPieces :: Position -> [(Square, Piece)]
blackPieces pos = (\t -> (fst t, fromJust (snd t))) <$> filter isBlack (Map.assocs pos)

whiteToPlay :: GameHistory -> Bool
whiteToPlay = odd . length

toPlay :: GameHistory -> Color
toPlay pos = if whiteToPlay pos then White else Black

positionTree' :: GameHistory -> [GameHistory]
positionTree' gh = fmap (\p -> p : gh) $ positionTree gh

positionTree :: GameHistory -> [Position]
positionTree gh = filter (\p -> not $ isInCheck (p : gh) (toPlay gh)) $ positionTreeIgnoreCheck gh

positionTreeIgnoreCheck :: GameHistory -> [Position]
positionTreeIgnoreCheck gh
    | whiteToPlay gh = (whitePieces (head gh) >>= positionsPrPiece gh >>= promoteBindFriendly White) ++ castle gh
    | otherwise = (blackPieces (head gh) >>= positionsPrPiece gh >>= promoteBindFriendly Black) ++ castle gh

positionTreeIgnoreCheckPromotionsCastle :: GameHistory -> Color -> [Position]
positionTreeIgnoreCheckPromotionsCastle gh White = whitePieces (head gh) >>= positionsPrPiece gh
positionTreeIgnoreCheckPromotionsCastle gh Black = blackPieces (head gh) >>= positionsPrPiece gh

positionsPrPiece :: GameHistory -> (Square, Piece) -> [Position]
positionsPrPiece gh (s,p) = case p of (Pawn _) -> fmap (\t -> movePiece (eliminateEnPassantSquare pos t) s (fst t)) (filter (\t -> canGoThere pos s (fst t)) $ toSquaresPawn gh (s, p))
                                      (Knight _) -> fmap (movePiece pos s) (filter (finalDestinationNotOccupiedBySelf pos s) $ toSquaresKnight s)
                                      (Bishop _) -> fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresBishop s)
                                      (Rook _) -> fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresRook s)
                                      (Queen _) -> fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresQueen s)
                                      (King _) -> fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresKing s)
  where pos = head gh

eliminateEnPassantSquare :: Position -> (Square, Maybe Square) -> Position
eliminateEnPassantSquare pos (_, Nothing) = pos
eliminateEnPassantSquare pos (_, Just s2) = removePieceAt pos s2

-- pawns - returns new squares, along with an optional capture square (because of en passant)
toSquaresPawn :: GameHistory -> (Square, Piece) -> [(Square, Maybe Square)]
toSquaresPawn gh (s, p)
        | colr p == White = filter insideBoard' $
            [(squareTo s 0 2, Nothing) | snd s == 2, vacantAt pos $ squareTo s 0 2] ++
            [(squareTo s 0 1, Nothing) | vacantAt pos $ squareTo s 0 1] ++
            [(squareTo s (-1) 1, Nothing) | enemyAt pos s $ squareTo s (-1) 1] ++
            [(squareTo s (-1) 1, Just (squareTo s (-1) 0)) | enPassant gh (squareTo s (-1) 0)] ++
            [(squareTo s 1 1, Nothing) | enemyAt pos s $ squareTo s 1 1] ++
            [(squareTo s 1 1, Just (squareTo s 1 0)) | enPassant gh (squareTo s 1 0)]
        | otherwise = filter insideBoard' $
            [(squareTo s 0 (-2), Nothing) | snd s == 7, vacantAt pos $ squareTo s 0 (-2)] ++
            [(squareTo s 0 (-1), Nothing) | vacantAt pos $ squareTo s 0 (-1)] ++
            [(squareTo s (-1) (-1), Nothing) | enemyAt pos s $ squareTo s (-1) (-1)] ++
            [(squareTo s (-1) (-1), Just (squareTo s (-1) 0)) | enPassant gh (squareTo s (-1) 0)] ++
            [(squareTo s 1 (-1), Nothing) | enemyAt pos s $ squareTo s 1 (-1)] ++
            [(squareTo s 1 (-1), Just (squareTo s 1 0)) | enPassant gh (squareTo s 1 0)] -- bug
  where pos = head gh

-- en passant
enPassant :: GameHistory -> Square -> Bool
enPassant [] _ = False
enPassant gh s
  | toPlay gh == White = (snd s == 5) && pieceAt (head gh) s == Just (Pawn Black) && wasLastPieceToMove gh s
  | otherwise = (snd s == 7) && pieceAt (head gh) s == Just (Pawn White) && wasLastPieceToMove gh s
    where toCol = if toPlay gh == White then (fst s, 7) else (fst s, 2)
          wasLastPieceToMove gh' s' = movePiece (head gh) s' toCol == (head . tail) gh'

-- promotions :: promote one position
prom :: Color -> Piece -> (Square, Maybe Piece) -> (Square, Maybe Piece)
prom White p (s, mp) = if snd s == 8 && mp == Just (Pawn White) then (s, Just p) else (s, mp)
prom Black p (s, mp) = if snd s == 1 && mp == Just (Pawn Black) then (s, Just p) else (s, mp)

-- promote one position
promoteTo :: Color -> Position -> Piece -> Position
promoteTo c pos p = Map.fromList $ fmap (prom c p) (Map.toList pos)

-- promote one position to [] or all four positions
maybePromote :: Color -> Position -> Piece -> [Position]
maybePromote c pos p = [promoteTo c pos p | canPromote c pos p]
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

type KingPos = Color -> Square
type RookPos = Color -> Square
type PerformCastleF = GameHistory -> Color -> Position

castle :: GameHistory -> [Position]
castle gh = castleShort gh (toPlay gh) ++ castleLong gh (toPlay gh)

castleShort :: GameHistory -> Color -> [Position]
castleShort gh color = castle' gh color kingPos shortRookPos doCastleShort

castleLong :: GameHistory -> Color -> [Position]
castleLong gh color = castle' gh color kingPos longRookPos doCastleLong

castle' :: GameHistory -> Color -> KingPos -> RookPos -> PerformCastleF -> [Position]
castle' gh color kingPosF rookPosF doCastleF = if
   pieceAt (head gh) (kingPosF color) == Just (king color) && -- must have a king at home
   pieceAt (head gh) (rookPosF color) == Just (rook color) && -- must have a rook at home
   vacantBetween gh (kingPosF color) (rookPosF color) && -- must be vacant between king and rook
   haveNotMoved gh (king color) (kingPosF color) && -- must not have moved king
   haveNotMoved gh (rook color) (rookPosF color) && -- must not have moved rook
   (not (isInCheck gh color) && -- must not be in check
   willNotPassCheck gh (kingPosF color) (rookPosF color)) -- must not move through check
     then [doCastleF gh color]
     else []

doCastleShort :: GameHistory -> Color -> Position
doCastleShort gh White = replacePieceAt (replacePieceAt (removePieceAt (removePieceAt (head gh) ('e', 1)) ('h', 1)) ('g', 1) (King White)) ('f', 1) (Rook White)
doCastleShort gh Black = replacePieceAt (replacePieceAt (removePieceAt (removePieceAt (head gh) ('e', 8)) ('h', 8)) ('g', 8) (King Black)) ('f', 8) (Rook Black)

doCastleLong :: GameHistory -> Color -> Position
doCastleLong gh White = replacePieceAt (replacePieceAt (removePieceAt (removePieceAt (head gh) ('e', 1)) ('a', 1)) ('c', 1) (King White)) ('d', 1) (Rook White)
doCastleLong gh Black = replacePieceAt (replacePieceAt (removePieceAt (removePieceAt (head gh) ('e', 8)) ('a', 8)) ('c', 8) (King Black)) ('d', 8) (Rook Black)

vacantBetween :: GameHistory -> Square -> Square -> Bool
vacantBetween gh from to = all (vacantAt (head gh)) $ points from to

kingPos :: Color -> Square
kingPos White = ('e',1)
kingPos Black = ('e',8)

shortRookPos :: Color -> Square
shortRookPos White = ('h', 1)
shortRookPos Black = ('h', 8)

longRookPos :: Color -> Square
longRookPos White = ('a', 1)
longRookPos Black = ('a', 8)

haveNotMoved :: GameHistory -> Piece -> Square -> Bool
haveNotMoved gh p s = all (\pos -> pieceAt pos s == Just p) gh

willNotPassCheck :: GameHistory -> Square -> Square -> Bool
willNotPassCheck gh ('e',1) ('h',1) = not (isInCheck (movePiece (head gh) ('e', 1) ('f', 1) : gh) (toPlay gh)) && not (isInCheck (movePiece (head gh) ('e', 1) ('g', 1) : gh) (toPlay gh))
willNotPassCheck gh ('e',1) ('a',1) = not (isInCheck (movePiece (head gh) ('e', 1) ('d', 1) : gh) (toPlay gh)) && not (isInCheck (movePiece (head gh) ('e', 1) ('c', 1) : gh) (toPlay gh))
willNotPassCheck gh ('e',8) ('h',8) = not (isInCheck (movePiece (head gh) ('e', 8) ('f', 8) : gh) (toPlay gh)) && not (isInCheck (movePiece (head gh) ('e', 8) ('g', 8) : gh) (toPlay gh))
willNotPassCheck gh ('e',8) ('a',8) = not (isInCheck (movePiece (head gh) ('e', 8) ('d', 8) : gh) (toPlay gh)) && not (isInCheck (movePiece (head gh) ('e', 8) ('c', 8) : gh) (toPlay gh))
willNotPassCheck _ s1 s2 = error $ "cannot use squares " ++ show s1 ++ " and " ++ show s2 ++ " as castling squares"

insideBoard :: Square -> Bool
insideBoard s = snd s >= 1 && snd s <= 8 && fst s >= 'a' && fst s <= 'h'

insideBoard' :: (Square, Maybe Square) -> Bool
insideBoard' (s, Nothing) = snd s >= 1 && snd s <= 8 && fst s >= 'a' && fst s <= 'h'
insideBoard' (s, Just s2) = insideBoard s && insideBoard s2

isInCheck :: GameHistory -> Color -> Bool
isInCheck gh clr = anyPosWithoutKing clr (positionTreeIgnoreCheckPromotionsCastle gh (succ' clr))

isCheckMate :: GameHistory -> Bool
isCheckMate gh  = isInCheck gh (toPlay gh) && null (positionTree gh)

isDraw :: GameHistory -> Bool
isDraw gh = isPatt gh || threefoldrepetition gh

threefoldrepetition :: GameHistory -> Bool
threefoldrepetition gh = max' (fmap snd $ posrep gh) > 2

max' :: Ord a => [a] -> a
max' [] = error "no max element in empty list"
max' [x] = x
max' (x:xs) = if x > max' xs then x else max' xs

posrep :: GameHistory -> [(Position, Int)]
posrep [] = [(Chess.emptyBoard, 1)]
posrep (x:xs) = (x, countHead it) : posrep (snd it)
  where it = partition (eqPosition x) (x:xs)
        countHead z = length $ fst z

eqPosition :: Position -> Position -> Bool
eqPosition p1 p2 = p1 == p2

isPatt :: GameHistory -> Bool
isPatt gh = not (isInCheck gh (toPlay gh)) && null (positionTree gh)

anyPosWithoutKing :: Color -> [Position] -> Bool
anyPosWithoutKing col pos = not $ allHasKing col pos

allHasKing :: Color -> [Position] -> Bool
allHasKing White poses = all (any (\(_, p) -> p == King White) . whitePieces) poses
allHasKing Black poses = all (any (\(_, p) -> p == King Black) . blackPieces) poses

determineStatus :: GameHistory -> Status
determineStatus gh
  | toPlay gh == White && isCheckMate gh = WhiteIsMate
  | isCheckMate gh = BlackIsMate
  | isDraw gh = Remis
  | toPlay gh == White = WhiteToPlay
  | otherwise = BlackToPlay