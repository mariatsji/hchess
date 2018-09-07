{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Chess
  ( board
  , onlyPiecesBoard
  , Color(..)
  , Piece(..)
  , Square(..)
  , Position(..)
  , GameHistory
  , Status(..)
  , startPosition
  , movePiece
  , makeMoves
  , removePieceAt
  , whitePieces
  , blackPieces
  , emptyBoard
  , replacePieceAt
  , positionTree
  , positionTree'
  , positionTreeIgnoreCheck
  , enPassant
  , canGoThere
  , finalDestinationNotOccupiedBySelf
  , points
  , points'
  , eqPosition
  , positionsPrPiece
  , to'
  , toSquaresPawn
  , pieceAt
  , toPlay
  , whiteToPlay
  , colr
  , isInCheck
  , anyPosWithoutKing
  , isCheckMate
  , isPatt
  , threefoldrepetition
  , posrep
  , isDraw
  , succ'
  , promote
  , promoteTo
  , promoteBindFriendly
  , castle
  , castleShort
  , castleLong
  , determineStatus
  ) where

import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad
import           Data.Char
import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set        as Set
import           Data.Tuple
import           GHC.Generics (Generic)

data Color
  = White
  | Black
  deriving (Eq, Ord, Enum, Show, Generic, NFData)

data Piece
  = Pawn Color
  | Knight Color
  | Bishop Color
  | Rook Color
  | Queen Color
  | King Color
  deriving (Eq, Ord, Show, Generic, NFData)

data Square = Square Int Int deriving (Eq, Ord, Show, Generic, NFData)

newtype Position = Position
  { m :: Map.Map Square Piece
  } deriving (Eq, Show, Generic, NFData)

type GameHistory = [Position]

data Status
  = WhiteToPlay
  | BlackToPlay
  | Remis
  | WhiteIsMate
  | BlackIsMate
  deriving (Eq, Ord, Show, Generic, NFData)

board :: [Square]
board = Square <$> [1 .. 8] <*> [1 .. 8]

onlyPiecesBoard :: [Square]
onlyPiecesBoard = Square <$> [1 .. 8] <*> [1,2,7,8]

colr :: Piece -> Color
colr (Pawn c)   = c
colr (Knight c) = c
colr (Bishop c) = c
colr (Rook c)   = c
colr (Queen c)  = c
colr (King c)   = c

king :: Color -> Piece
king White = King White
king Black = King Black

rook :: Color -> Piece
rook White = Rook White
rook Black = Rook Black

unique :: Eq a => [a] -> [a]
unique =
  foldl
    (\a c ->
       if c `elem` a
         then a
         else c : a)
    []

squareTo :: Square -> Int -> Int -> Square
squareTo (Square c r) cols rows = Square (c + cols) (r + rows)

startPosition :: Position
startPosition =
  Position $
  Map.fromList $
  [ (Square 1 1, Rook White)
  , (Square 2 1, Knight White)
  , (Square 3 1, Bishop White)
  , (Square 4 1, Queen White)
  , (Square 5 1, King White)
  , (Square 6 1, Bishop White)
  , (Square 7 1, Knight White)
  , (Square 8 1, Rook White)

  , (Square 1 2, Pawn White)
  , (Square 2 2, Pawn White)
  , (Square 3 2, Pawn White)
  , (Square 4 2, Pawn White)
  , (Square 5 2, Pawn White)
  , (Square 6 2, Pawn White)
  , (Square 7 2, Pawn White)
  , (Square 8 2, Pawn White)

  , (Square 1 7, Pawn Black)
  , (Square 2 7, Pawn Black)
  , (Square 3 7, Pawn Black)
  , (Square 4 7, Pawn Black)
  , (Square 5 7, Pawn Black)
  , (Square 6 7, Pawn Black)
  , (Square 7 7, Pawn Black)
  , (Square 8 7, Pawn Black)

  , (Square 1 8, Rook Black)
  , (Square 2 8, Knight Black)
  , (Square 3 8, Bishop Black)
  , (Square 4 8, Queen Black)
  , (Square 5 8, King Black)
  , (Square 6 8, Bishop Black)
  , (Square 7 8, Knight Black)
  , (Square 8 8, Rook Black)
  ]

emptyBoard :: Position
emptyBoard = Position $ Map.empty

movePiece :: Position -> Square -> Square -> Position
movePiece pos from@(Square fc fr) to@(Square tc tr)
  | pieceAt pos from == Just (Pawn White) &&
      (fc /= tc) && vacantAt pos to =
    movePiece' (removePieceAt pos (Square tc (tr - 1))) from to
  | pieceAt pos from == Just (Pawn Black) &&
      (fc /= tc) && vacantAt pos to =
    movePiece' (removePieceAt pos (Square tc (tr + 1))) from to
  | otherwise = movePiece' pos from to

movePiece' :: Position -> Square -> Square -> Position
movePiece' pos from to =
  case pieceAt pos from of
    (Just piece) -> replacePieceAt (removePieceAt pos from) to piece
    Nothing      -> pos

points :: Square -> Square -> [Square]
points (Square c1 r1) (Square c2 r2) =
  let toSquare (c,r) = Square c r
  in toSquare <$> points' (c1, r1) (c2, r2)

points' :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -- all visited squares
points' (c1, r1) (c2, r2)
  | c1 == c2 || r1 == r2 =
      middle $ (,) <$> to' c1 c2 <*> to' r1 r2
  | otherwise =
      filter (\(a, b) -> (a, b) /= (c1, r1) && (a, b) /= (c2, r2)) $
      zip (c1 `to'` c2) (r1 `to'` r2)

middle :: [a] -> [a]
middle l = if length l > 1 then (reverse . init . reverse . init) l else []

to' :: Int -> Int -> [Int]
to' a b
  | a == b = [a]
  | a > b = a : to' (a - 1) b
  | otherwise = a : to' (a + 1) b

canGoThere :: Position -> Square -> Square -> Bool
canGoThere pos from to =
  all isNothing (fmap (pieceAt pos) (points from to)) &&
  finalDestinationNotOccupiedBySelf pos from to

finalDestinationNotOccupiedBySelf :: Position -> Square -> Square -> Bool
finalDestinationNotOccupiedBySelf pos f t =
  fmap colr (pieceAt pos t) /= fmap colr (pieceAt pos f)

enemyAt :: Position -> Square -> Square -> Bool
enemyAt pos f t =
  fmap (succ' . colr) (pieceAt pos t) == fmap colr (pieceAt pos f)

succ' :: Color -> Color
succ' White = Black
succ' Black = White

vacantAt :: Position -> Square -> Bool
vacantAt pos t = isNothing $ pieceAt pos t

removePieceAt :: Position -> Square -> Position
removePieceAt (Position pos) square =
  Position $ Map.delete square pos

replacePieceAt :: Position -> Square -> Piece -> Position
replacePieceAt (Position pos) square piece =
  Position $ Map.insert square piece pos

makeMoves :: GameHistory -> [(Square, Square)] -> GameHistory
makeMoves gh []     = gh
makeMoves gh (x:xs) = makeMoves (movePiece (head gh) (fst x) (snd x) : gh) xs

pieceAt :: Position -> Square -> Maybe Piece
pieceAt (Position pos) square = pos Map.!? square

whitePieces :: Position -> [(Square, Piece)]
whitePieces (Position pos) = Map.foldMapWithKey f pos
  where f :: Square -> Piece -> [(Square, Piece)]
        f s p
          | colr p == White = [(s, p)]
          | otherwise = []

isWhite :: (Square, Maybe Piece) -> Bool
isWhite t =
  let mp = snd t
      p = maybe Black colr mp
   in p == White

-- cannot be changed to `not . isWhite` because of `Nothing` case
isBlack :: (Square, Maybe Piece) -> Bool
isBlack t =
  let mp = snd t
      p = maybe White colr mp
   in p == Black

blackPieces :: Position -> [(Square, Piece)]
blackPieces (Position pos) = Map.foldMapWithKey f pos
  where f :: Square -> Piece -> [(Square, Piece)]
        f s p
          | colr p == Black = [(s, p)]
          | otherwise = []

whiteToPlay :: GameHistory -> Bool
whiteToPlay = odd . length

toPlay :: GameHistory -> Color
toPlay pos =
  if whiteToPlay pos
    then White
    else Black

type Move = Square -> Square

positionTree' :: GameHistory -> [GameHistory]
positionTree' gh = (: gh) <$> positionTree gh

positionTree :: GameHistory -> [Position]
positionTree gh =
  let player = toPlay gh
      notInCheck = not . flip isInCheck player . (: gh)
      completePT = positionTreeIgnoreCheck gh
  in filter notInCheck completePT

positionTreeIgnoreCheck :: GameHistory -> [Position]
positionTreeIgnoreCheck gh
  | whiteToPlay gh =
    (whitePieces (head gh) >>= positionsPrPiece gh >>= promoteBindFriendly White) ++
    castle gh
  | otherwise =
    (blackPieces (head gh) >>= positionsPrPiece gh >>= promoteBindFriendly Black) ++
    castle gh

positionTreeIgnoreCheckPromotionsCastle :: GameHistory -> Color -> [Position]
positionTreeIgnoreCheckPromotionsCastle gh White =
  whitePieces (head gh) >>= positionsPrPiece gh
positionTreeIgnoreCheckPromotionsCastle gh Black =
  blackPieces (head gh) >>= positionsPrPiece gh

positionsPrPiece :: GameHistory -> (Square, Piece) -> [Position]
positionsPrPiece gh (s, p) =
  case p of
    (Pawn _) ->
      fmap
        (\t -> movePiece (eliminateEnPassantSquare pos t) s (fst t))
        (filter (\t -> canGoThere pos s (fst t)) $ toSquaresPawn gh (s, p))
    (Knight _) ->
      fmap
        (movePiece pos s)
        (filter (finalDestinationNotOccupiedBySelf pos s) $ toSquaresKnight s)
    (Bishop _) ->
      fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresBishop s)
    (Rook _) ->
      fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresRook s)
    (Queen _) ->
      fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresQueen s)
    (King _) ->
      fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresKing s)
  where
    pos = head gh

eliminateEnPassantSquare :: Position -> (Square, Maybe Square) -> Position
eliminateEnPassantSquare pos (_, Nothing) = pos
eliminateEnPassantSquare pos (_, Just s2) = removePieceAt pos s2

-- pawns - returns new squares, along with an optional capture square (because of en passant)
toSquaresPawn :: GameHistory -> (Square, Piece) -> [(Square, Maybe Square)]
toSquaresPawn gh (s@(Square c r), p)
  | colr p == White =
    filter insideBoard' $
    [(squareTo s 0 2, Nothing) | r == 2, vacantAt pos $ squareTo s 0 2] ++
    [(squareTo s 0 1, Nothing) | vacantAt pos $ squareTo s 0 1] ++
    [(squareTo s (-1) 1, Nothing) | enemyAt pos s $ squareTo s (-1) 1] ++
    [ (squareTo s (-1) 1, Just (squareTo s (-1) 0))
    | enPassant gh (squareTo s (-1) 0)
    ] ++
    [(squareTo s 1 1, Nothing) | enemyAt pos s $ squareTo s 1 1] ++
    [(squareTo s 1 1, Just (squareTo s 1 0)) | enPassant gh (squareTo s 1 0)]
  | otherwise =
    filter insideBoard' $
    [ (squareTo s 0 (-2), Nothing)
    | r == 7
    , vacantAt pos $ squareTo s 0 (-2)
    ] ++
    [(squareTo s 0 (-1), Nothing) | vacantAt pos $ squareTo s 0 (-1)] ++
    [(squareTo s (-1) (-1), Nothing) | enemyAt pos s $ squareTo s (-1) (-1)] ++
    [ (squareTo s (-1) (-1), Just (squareTo s (-1) 0))
    | enPassant gh (squareTo s (-1) 0)
    ] ++
    [(squareTo s 1 (-1), Nothing) | enemyAt pos s $ squareTo s 1 (-1)] ++
    [(squareTo s 1 (-1), Just (squareTo s 1 0)) | enPassant gh (squareTo s 1 0)] -- bug
  where
    pos = head gh

-- en passant
enPassant :: GameHistory -> Square -> Bool
enPassant [] _ = False
enPassant gh s@(Square c r)
  | toPlay gh == White =
    (r == 5) &&
    pieceAt (head gh) s == Just (Pawn Black) && wasLastPieceToMove gh s
  | otherwise =
    (r == 7) &&
    pieceAt (head gh) s == Just (Pawn White) && wasLastPieceToMove gh s
  where
    toSquare =
      if toPlay gh == White
        then Square c 7
        else Square c 2
    wasLastPieceToMove gh' s' =
      movePiece (head gh) s' toSquare == (head . tail) gh'

prom :: Color -> Piece -> (Square, Piece) -> (Square, Piece)
prom White p1 (s@(Square c r), p2) = if r == 8 && p2 == Pawn White then (s, p1) else (s, p2)
prom Black p1 (s@(Square c r), p2) = if r == 1 && p2 == Pawn Black then (s, p1) else (s, p2)

-- promote one position
promoteTo :: Color -> Position -> Piece -> Position
promoteTo c (Position pos) p =
  Position $ Map.fromList $ fmap (prom c p) (Map.toList pos)

-- promote one position to [] or all four positions
maybePromote :: Color -> Position -> Piece -> [Position]
maybePromote c pos p = [promoteTo c pos p | canPromote c pos p]
  where
    canPromote c' pos' p' = promoteTo c' pos' p' /= pos'

promote :: Color -> Position -> [Position]
promote c@White pos =
  maybePromote c pos (Queen White) ++
  maybePromote c pos (Rook White) ++
  maybePromote c pos (Bishop White) ++ maybePromote c pos (Knight White)
promote c@Black pos =
  maybePromote c pos (Queen Black) ++
  maybePromote c pos (Rook Black) ++
  maybePromote c pos (Bishop Black) ++ maybePromote c pos (Knight Black)

-- optimization, only check for promotions with pending pawns
promoteBindFriendly :: Color -> Position -> [Position]
promoteBindFriendly White pos =
  if elem (Just (Pawn White)) [pieceAt pos (Square col 8)  | col <- [1 .. 8]]
    then promoteBindFriendly' White pos
    else [pos]
promoteBindFriendly Black pos =
  if elem (Just (Pawn Black)) [pieceAt pos (Square col 1) | col <- [1 .. 8]]
    then promoteBindFriendly' Black pos
    else [pos]

-- same pos or all four
promoteBindFriendly' :: Color -> Position -> [Position]
promoteBindFriendly' c pos =
  if promote c pos /= [pos] && promote c pos /= []
    then promote c pos
    else [pos]

-- knights
toSquaresKnight :: Square -> [Square]
toSquaresKnight s =
  filter
    insideBoard
    [ squareTo s (-1) 2
    , squareTo s (-1) (-2)
    , squareTo s 1 2
    , squareTo s 1 (-2)
    , squareTo s 2 1
    , squareTo s 2 (-1)
    , squareTo s (-2) 1
    , squareTo s (-2) (-1)
    ]

-- bishops
toSquaresBishop :: Square -> [Square]
toSquaresBishop s =
  unique
    [ squareTo s a b
    | a <- [-7 .. 7]
    , b <- [-7 .. 7]
    , abs a == abs b
    , (a, b) /= (0, 0)
    , insideBoard $ squareTo s a b
    ]

-- rooks
toSquaresRook :: Square -> [Square]
toSquaresRook s =
  unique
    [ squareTo s a b
    | a <- [-7 .. 7]
    , b <- [-7 .. 7]
    , insideBoard $ squareTo s a b
    , a == 0 || b == 0
    , (a, b) /= (0, 0)
    ]

-- queens
toSquaresQueen :: Square -> [Square]
toSquaresQueen s = toSquaresBishop s `mappend` toSquaresRook s

-- kings
toSquaresKing :: Square -> [Square]
toSquaresKing s =
  [ squareTo s a b
  | a <- [-1, 0, 1]
  , b <- [-1, 0, 1]
  , (a, b) /= (0, 0)
  , insideBoard $ squareTo s a b
  ]

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

castle' ::
     GameHistory -> Color -> KingPos -> RookPos -> PerformCastleF -> [Position]
castle' gh color kingPosF rookPosF doCastleF =
  if pieceAt (head gh) (kingPosF color) == Just (king color) && -- must have a king at home
     pieceAt (head gh) (rookPosF color) == Just (rook color) && -- must have a rook at home
     vacantBetween gh (kingPosF color) (rookPosF color) && -- must be vacant between king and rook
     haveNotMoved gh (king color) (kingPosF color) && -- must not have moved king
     haveNotMoved gh (rook color) (rookPosF color) && -- must not have moved rook
     (not (isInCheck gh color) && -- must not be in check
      willNotPassCheck gh (kingPosF color) (rookPosF color) -- must not move through check
      )
    then [doCastleF gh color]
    else []

doCastleShort :: GameHistory -> Color -> Position
doCastleShort gh White =
  replacePieceAt
    (replacePieceAt
       (removePieceAt (removePieceAt (head gh) (Square 5 1)) (Square 8 1))
       (Square 7 1)
       (King White))
    (Square 6 1)
    (Rook White)
doCastleShort gh Black =
  replacePieceAt
    (replacePieceAt
       (removePieceAt (removePieceAt (head gh) (Square 5 8)) (Square 8 8))
       (Square 7 8)
       (King Black))
    (Square 6 8)
    (Rook Black)

doCastleLong :: GameHistory -> Color -> Position
doCastleLong gh White =
  replacePieceAt
    (replacePieceAt
       (removePieceAt (removePieceAt (head gh) (Square 5 1)) (Square 1 1))
       (Square 3 1)
       (King White))
    (Square 4 1)
    (Rook White)
doCastleLong gh Black =
  replacePieceAt
    (replacePieceAt
       (removePieceAt (removePieceAt (head gh) (Square 5 8)) (Square 1 8))
       (Square 3 8)
       (King Black))
    (Square 4 8)
    (Rook Black)

vacantBetween :: GameHistory -> Square -> Square -> Bool
vacantBetween gh from to = all (vacantAt (head gh)) $ points from to

kingPos :: Color -> Square
kingPos White = Square 5 1
kingPos Black = Square 5 8

shortRookPos :: Color -> Square
shortRookPos White = Square 8 1
shortRookPos Black = Square 8 8

longRookPos :: Color -> Square
longRookPos White = Square 1 1
longRookPos Black = Square 1 8

haveNotMoved :: GameHistory -> Piece -> Square -> Bool
haveNotMoved gh p s = all (\pos -> pieceAt pos s == Just p) gh

willNotPassCheck :: GameHistory -> Square -> Square -> Bool
willNotPassCheck gh (Square 5 1) (Square 8 1) =
  not (isInCheck (movePiece (head gh) (Square 5 1) (Square 6 1) : gh) (toPlay gh)) &&
  not (isInCheck (movePiece (head gh) (Square 5 1) (Square 7 1) : gh) (toPlay gh))
willNotPassCheck gh (Square 5 1) (Square 1 1) =
  not (isInCheck (movePiece (head gh) (Square 5 1) (Square 4 1) : gh) (toPlay gh)) &&
  not (isInCheck (movePiece (head gh) (Square 5 1) (Square 3 1) : gh) (toPlay gh))
willNotPassCheck gh (Square 5 8) (Square 8 8) =
  not (isInCheck (movePiece (head gh) (Square 5 8) (Square 6 8) : gh) (toPlay gh)) &&
  not (isInCheck (movePiece (head gh) (Square 5 8) (Square 7 8) : gh) (toPlay gh))
willNotPassCheck gh (Square 5 8) (Square 1 8) =
  not (isInCheck (movePiece (head gh) (Square 5 8) (Square 4 8) : gh) (toPlay gh)) &&
  not (isInCheck (movePiece (head gh) (Square 5 8) (Square 3 8) : gh) (toPlay gh))
willNotPassCheck _ s1 s2 =
  error $
  "cannot use squares " ++
  show s1 ++ " and " ++ show s2 ++ " as castling squares"

insideBoard :: Square -> Bool
insideBoard (Square c r) = c >= 1 && c <= 8 && r >= 1 && r <= 8

insideBoard' :: (Square, Maybe Square) -> Bool
insideBoard' (s, Nothing) = insideBoard s
insideBoard' (s, Just s2) = insideBoard s && insideBoard s2

isInCheck :: GameHistory -> Color -> Bool
isInCheck gh clr =
  let potentialNextPositions = positionTreeIgnoreCheckPromotionsCastle gh (succ' clr)
      anyPWithoutKing = anyPosWithoutKing clr potentialNextPositions
  in seq anyPWithoutKing anyPWithoutKing
  --anyPosWithoutKing clr (positionTreeIgnoreCheckPromotionsCastle gh (succ' clr))

isCheckMate :: GameHistory -> Bool
isCheckMate gh = isInCheck gh (toPlay gh) && null (positionTree gh)

isDraw :: GameHistory -> Bool
isDraw gh = isPatt gh || threefoldrepetition gh

threefoldrepetition :: GameHistory -> Bool
threefoldrepetition gh = max' (fmap snd $ posrep gh) > 2

max' :: Ord a => [a] -> a
max' [] = error "no max element in empty list"
max' [x] = x
max' (x:xs) =
  if x > max' xs
    then x
    else max' xs

posrep :: GameHistory -> [(Position, Int)]
posrep [] = [(Chess.emptyBoard, 1)]
posrep (x:xs) = (x, countHead it) : posrep (snd it)
  where
    it = partition (eqPosition x) (x : xs)
    countHead z = length $ fst z

eqPosition :: Position -> Position -> Bool
eqPosition p1 p2 = p1 == p2

isPatt :: GameHistory -> Bool
isPatt gh = not (isInCheck gh (toPlay gh)) && null (positionTree gh)

anyPosWithoutKing :: Color -> [Position] -> Bool
anyPosWithoutKing col pos = not $ allHasKing col pos

allHasKing :: Color -> [Position] -> Bool
allHasKing White poses =
  all (any (\(_, p) -> p == King White) . whitePieces) poses
allHasKing Black poses =
  all (any (\(_, p) -> p == King Black) . blackPieces) poses

determineStatus :: GameHistory -> Status
determineStatus gh
  | toPlay gh == White && isCheckMate gh = WhiteIsMate
  | isCheckMate gh = BlackIsMate
  | isDraw gh = Remis
  | toPlay gh == White = WhiteToPlay
  | otherwise = BlackToPlay
