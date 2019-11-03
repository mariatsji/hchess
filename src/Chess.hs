{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Chess where

import Bunch
import Control.DeepSeq
import Data.List
import Data.Maybe
import GHC.Generics (Generic)
import Position
import Prelude hiding (foldr)

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
onlyPiecesBoard = Square <$> [1 .. 8] <*> [1, 2, 7, 8]

king :: Color -> Piece
king White = King White
king Black = King Black

rook :: Color -> Piece
rook White = Rook White
rook Black = Rook Black

squareTo :: Square -> Int -> Int -> Square
squareTo (Square c r) cols rows = Square (c + cols) (r + rows)

movePiece :: Position -> Square -> Square -> Position
movePiece pos@(Position m' _ _ _) from@(Square fc _) to@(Square tc tr)
  | pieceAt pos from == Just (Pawn White) && (fc /= tc) && vacantAt pos to =
    let newSnapshot =
          movePiece' (removePieceAt m' (Square tc (tr - 1))) from to
     in mkPosition pos newSnapshot (castleStatusWhite pos) (castleStatusBlack pos) 
  | pieceAt pos from == Just (Pawn Black) && (fc /= tc) && vacantAt pos to =
    let newSnapshot =
          movePiece' (removePieceAt m' (Square tc (tr + 1))) from to
     in mkPosition pos newSnapshot (castleStatusWhite pos) (castleStatusBlack pos)
  | otherwise =
    let newSnapshot = movePiece' m' from to
        newCastleStatusWhite = case toPlay pos of
          White -> mkNewCastleStatus pos White from
          Black -> castleStatusWhite pos
        newCastleStatusBlack = case toPlay pos of
          Black -> mkNewCastleStatus pos Black from
          White -> castleStatusBlack pos
     in mkPosition pos newSnapshot newCastleStatusWhite newCastleStatusBlack

mkNewCastleStatus :: Position -> Color -> Square -> CastleStatus
mkNewCastleStatus pos White from = case from of
  Square 1 1 -> case castleStatusWhite pos of
    CanCastleBoth -> CanCastleH
    CanCastleH -> CanCastleH
    _ -> CanCastleNone
  Square 5 1 -> CanCastleNone
  Square 8 1 -> case castleStatusWhite pos of
    CanCastleBoth -> CanCastleA
    CanCastleA -> CanCastleA
    _ -> CanCastleNone
  _ -> castleStatusWhite pos
mkNewCastleStatus pos Black from = case from of
  Square 1 8 -> case castleStatusBlack pos of
    CanCastleBoth -> CanCastleH
    CanCastleH -> CanCastleH
    _ -> CanCastleNone
  Square 5 8 -> CanCastleNone
  Square 8 8 -> case castleStatusBlack pos of
    CanCastleBoth -> CanCastleA
    CanCastleA -> CanCastleA
    _ -> CanCastleNone
  _ -> castleStatusBlack pos


points :: Square -> Square -> [Square]
points (Square c1 r1) (Square c2 r2) =
  let cline = line c1 c2
      rline = line r1 r2
   in if length cline > length rline
        then uncurry Square <$> zip cline (repeat r1)
        else
          if length rline > length cline
            then uncurry Square <$> zip (repeat c1) rline
            else uncurry Square <$> zip cline rline
  where
    line :: Int -> Int -> [Int]
    line a b =
      let step = if b > a then 1 else (-1)
       in [a + step, (a + step) + step .. b - step]

canGoThere :: Position -> Square -> Square -> Bool
canGoThere pos from to =
  all isNothing (fmap (pieceAt pos) (points from to))
    && finalDestinationNotOccupiedBySelf pos from to

finalDestinationNotOccupiedBySelf :: Position -> Square -> Square -> Bool
finalDestinationNotOccupiedBySelf pos f t =
  fmap colr (pieceAt pos t) /= fmap colr (pieceAt pos f)
  -- let pieces = unBunch $ snd <$> searchForPieces pos (\s -> s == f || s == t) (const True)
  -- in length pieces == 2 && colr (head pieces) == colr (pieces !! 1)

enemyAt :: Position -> Square -> Square -> Bool
enemyAt pos f t =
  fmap (succ' . colr) (pieceAt pos t) == fmap colr (pieceAt pos f)

succ' :: Color -> Color
succ' White = Black
succ' Black = White

vacantAt :: Position -> Square -> Bool
vacantAt pos t = isNothing $ pieceAt pos t

makeMoves :: Position -> [(Square, Square)] -> Position
makeMoves = foldl (\p x -> uncurry (movePiece p) x)

-- CAF now? would be nice
pieceAt :: Position -> Square -> Maybe Piece
pieceAt pos = pieceAt' (m pos)

whiteToPlay :: Position -> Bool
whiteToPlay = even . length . gamehistory

toPlay :: Position -> Color
toPlay pos = if whiteToPlay pos then White else Black

positionTree :: Position -> Bunch Position
positionTree pos =
  filter' (\p -> not $ isInCheck p (toPlay pos)) $! positionTreeIgnoreCheck pos

positionTreeIgnoreCheck :: Position -> Bunch Position
positionTreeIgnoreCheck pos =
  let c = toPlay pos
   in (searchForPieces pos (const True) (\p -> colr p == c) >>= positionsPrPiece pos >>= promoteBindFriendly c) <> castle pos

positionTreeIgnoreCheckPromotionsCastle :: Position -> Color -> Bunch Position
positionTreeIgnoreCheckPromotionsCastle pos c = searchForPieces pos (const True) (\p -> colr p == c) >>= positionsPrPiece pos

positionsPrPiece :: Position -> (Square, Piece) -> Bunch Position
positionsPrPiece pos@(Position snp _ _ _) (s, p) = case p of
  Pawn _ ->
    let potentials = filter (canGoThere pos s . fst) $ toSquaresPawn pos (s, p)
     in Bunch
          $ map
              ( \t -> case snd t of
                  Nothing -> movePiece pos s (fst t)
                  Just s' -> movePiece pos {m = removePieceAt snp s'} s (fst t)
                )
              potentials
  Knight _ ->
    Bunch
      $ fmap
          (movePiece pos s)
          (filter (finalDestinationNotOccupiedBySelf pos s) $ toSquaresKnight s)
  Bishop _ ->
    Bunch $ fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresBishop s)
  Rook _ ->
    Bunch $ fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresRook s)
  Queen _ ->
    Bunch $ fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresQueen s)
  King _ ->
    Bunch $ fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresKing s)

-- pawns - returns new squares, along with an optional capture square (because of en passant)
toSquaresPawn :: Position -> (Square, Piece) -> [(Square, Maybe Square)]
toSquaresPawn pos (s@(Square _ r), p)
  | colr p == White =
    filter insideBoard'
      $ [(squareTo s 0 2, Nothing) | r == 2, vacantAt pos $ squareTo s 0 2]
      <> [(squareTo s 0 1, Nothing) | vacantAt pos $ squareTo s 0 1]
      <> [(squareTo s (-1) 1, Nothing) | enemyAt pos s $ squareTo s (-1) 1]
      <> [(squareTo s (-1) 1, Just (squareTo s (-1) 0)) | enPassant pos (squareTo s (-1) 0)]
      <> [(squareTo s 1 1, Nothing) | enemyAt pos s $ squareTo s 1 1]
      <> [(squareTo s 1 1, Just (squareTo s 1 0)) | enPassant pos (squareTo s 1 0)]
  | otherwise =
    filter insideBoard'
      $ [(squareTo s 0 (-2), Nothing) | r == 7, vacantAt pos $ squareTo s 0 (-2)]
      <> [(squareTo s 0 (-1), Nothing) | vacantAt pos $ squareTo s 0 (-1)]
      <> [(squareTo s (-1) (-1), Nothing) | enemyAt pos s $ squareTo s (-1) (-1)]
      <> [(squareTo s (-1) (-1), Just (squareTo s (-1) 0)) | enPassant pos (squareTo s (-1) 0)]
      <> [(squareTo s 1 (-1), Nothing) | enemyAt pos s $ squareTo s 1 (-1)]
      <> [(squareTo s 1 (-1), Just (squareTo s 1 0)) | enPassant pos (squareTo s 1 0)]

-- en passant
enPassant :: Position -> Square -> Bool
enPassant (Position _ [] _ _) _ = False
enPassant pos s@(Square c r)
  | toPlay pos == White =
    (r == 5) && pieceAt pos s == Just (Pawn Black) && jumpedHereJustNow pos s
  | otherwise =
    (r == 4) && pieceAt pos s == Just (Pawn White) && jumpedHereJustNow pos s
  where
    piece = Just $ Pawn (toPlay pos)
    fromSquare = if toPlay pos == White then Square c 7 else Square c 2
    jumpedHereJustNow :: Position -> Square -> Bool
    jumpedHereJustNow _ _ =
      not $ length (gamehistory pos) < 3
        && let prevSnapshot = gamehistory pos !! 1
            in pieceAt' prevSnapshot fromSquare == piece && pieceAt' (m pos) s == piece && isNothing (pieceAt' prevSnapshot s)

promote :: Color -> Position -> [Position]
promote c pos = maybePromote c pos =<< [Queen c, Rook c, Bishop c, Knight c]

-- promote one position to [] or all four positions
maybePromote :: Color -> Position -> Piece -> [Position]
maybePromote c pos p = [promoteTo c pos p | canPromote c pos p]
  where
    canPromote c' pos' p' = promoteTo c' pos' p' /= pos'

-- promote one position
promoteTo :: Color -> Position -> Piece -> Position
promoteTo c pos p =
  let allPieces = searchForPieces pos (const True) (const True) -- Bunch (Square, Piece)
      listPromoted = fmap (prom c p) allPieces
   in mkPosition pos (fromList' (unBunch listPromoted)) (castleStatusWhite pos) (castleStatusBlack pos)

prom :: Color -> Piece -> (Square, Piece) -> (Square, Piece)
prom White p1 (s@(Square _ r), p2) =
  if r == 8 && p2 == Pawn White then (s, p1) else (s, Pawn White)
prom Black p1 (s@(Square _ r), p2) =
  if r == 1 && p2 == Pawn Black then (s, p1) else (s, Pawn Black)

-- optimization, only check for promotions with pending pawns
promoteBindFriendly :: Color -> Position -> Bunch Position
promoteBindFriendly c pos =
  Bunch
    $ if Just (Pawn c) `elem` [pieceAt pos (Square col (promoteRow c)) | col <- [1 .. 8]]
      then promoteBindFriendly' c pos
      else [pos]

-- same pos or all four
promoteBindFriendly' :: Color -> Position -> [Position]
promoteBindFriendly' c pos =
  if promote c pos /= [pos] && promote c pos /= [] then promote c pos else [pos]

-- knights
toSquaresKnight :: Square -> [Square]
toSquaresKnight s =
  filter
    insideBoard
    [ squareTo s (-1) 2,
      squareTo s (-1) (-2),
      squareTo s 1 2,
      squareTo s 1 (-2),
      squareTo s 2 1,
      squareTo s 2 (-1),
      squareTo s (-2) 1,
      squareTo s (-2) (-1)
      ]

-- bishops
toSquaresBishop :: Square -> [Square]
toSquaresBishop s@(Square c r) =
  let maxDown = r - 1
      maxUp = 8 - r
      maxLeft = c - 1
      maxRight = 8 - c
      a' = fmap (\x -> squareTo s x x) [1 .. min maxUp maxRight]
      b' = fmap (\x -> squareTo s x (- x)) [1 .. min maxDown maxRight]
      c' = fmap (\x -> squareTo s (- x) (- x)) [1 .. min maxDown maxLeft]
      d' = fmap (\x -> squareTo s (- x) x) [1 .. min maxLeft maxUp]
   in a' <> b' <> c' <> d'

-- rooks
toSquaresRook :: Square -> [Square]
toSquaresRook s@(Square c r) =
  let maxDown = 1 - r
      maxUp = 8 - r
      maxLeft = 1 - c
      maxRight = 8 - c
      lane = fmap (squareTo s 0) [maxDown .. maxUp]
      row = fmap (\c' -> squareTo s c' 0) [maxLeft .. maxRight]
   in lane <> row

-- queens
toSquaresQueen :: Square -> [Square]
toSquaresQueen s = toSquaresBishop s <> toSquaresRook s

-- kings
toSquaresKing :: Square -> [Square]
toSquaresKing s =
  [ squareTo s a b
    | a <- [-1, 0, 1],
      b <- [-1, 0, 1],
      (a, b) /= (0, 0),
      insideBoard $ squareTo s a b
    ]

-- castles
type KingPos = Color -> Square

type RookPos = Color -> Square

type PerformCastleF = Snapshot -> Color -> Snapshot

castle :: Position -> Bunch Position
castle pos =
  case toPlay pos of
    White -> case castleStatusWhite pos of
      CanCastleNone -> Bunch []
      CanCastleBoth -> Bunch $ castleShort pos White <> castleLong pos White
      CanCastleA -> Bunch $ castleLong pos White
      CanCastleH -> Bunch $ castleShort pos White
    Black -> case castleStatusBlack pos of
      CanCastleNone -> Bunch []
      CanCastleBoth -> Bunch $ castleShort pos Black <> castleLong pos Black
      CanCastleA -> Bunch $ castleLong pos Black
      CanCastleH -> Bunch $ castleShort pos Black

castleShort :: Position -> Color -> [Position]
castleShort pos color = castle' pos color kingPos shortRookPos doCastleShort

castleLong :: Position -> Color -> [Position]
castleLong pos color = castle' pos color kingPos longRookPos doCastleLong

castle'
  :: Position -> Color -> KingPos -> RookPos -> PerformCastleF -> [Position]
castle' pos color kingPosF rookPosF doCastleF =
  if pieceAt pos (kingPosF color) == Just (king color) -- must have a king at home
    && pieceAt pos (rookPosF color) -- must have a rook at home
       == Just (rook color)
    && vacantBetween pos (kingPosF color) (rookPosF color) -- must be vacant between king and rook
    && ( not (isInCheck pos color) -- must not be in check
           && willNotPassCheck pos (kingPosF color) (rookPosF color) -- must not move through check
         )
    then
      let newSnapshot = doCastleF (m pos) color
          newCastleStatusWhite = if color == White then CanCastleNone else castleStatusWhite pos
          newCastleStatusBlack = if color == Black then CanCastleNone else castleStatusBlack pos
       in [mkPosition pos newSnapshot newCastleStatusWhite newCastleStatusBlack]
    else []

doCastleShort :: Snapshot -> Color -> Snapshot
doCastleShort pos c =
  replacePieceAt
    ( replacePieceAt
        (removePieceAt (removePieceAt pos (Square 5 1)) (Square 8 1))
        (Square 7 (homeRow c))
        (King c)
      )
    (Square 6 (homeRow c))
    (Rook c)

doCastleLong :: Snapshot -> Color -> Snapshot
doCastleLong pos c =
  replacePieceAt
    ( replacePieceAt
        (removePieceAt (removePieceAt pos (Square 5 1)) (Square 1 1))
        (Square 3 (homeRow c))
        (King c)
      )
    (Square 4 (homeRow c))
    (Rook c)

vacantBetween :: Position -> Square -> Square -> Bool
vacantBetween pos from to = all (vacantAt pos) $ points from to

kingPos :: Color -> Square
kingPos c = Square 5 (homeRow c)

shortRookPos :: Color -> Square
shortRookPos c = Square 8 (homeRow c)

longRookPos :: Color -> Square
longRookPos c = Square 1 (homeRow c)

homeRow :: Color -> Int
homeRow White = 1
homeRow Black = 8

promoteRow :: Color -> Int
promoteRow White = homeRow Black
promoteRow Black = homeRow White

willNotPassCheck :: Position -> Square -> Square -> Bool
willNotPassCheck pos (Square 5 1) (Square 8 1) =
  not (isInCheck (movePiece pos (Square 5 1) (Square 6 1)) (toPlay pos))
    && not (isInCheck (movePiece pos (Square 5 1) (Square 7 1)) (toPlay pos))
willNotPassCheck pos (Square 5 1) (Square 1 1) =
  not (isInCheck (movePiece pos (Square 5 1) (Square 4 1)) (toPlay pos))
    && not (isInCheck (movePiece pos (Square 5 1) (Square 3 1)) (toPlay pos))
willNotPassCheck pos (Square 5 8) (Square 8 8) =
  not (isInCheck (movePiece pos (Square 5 8) (Square 6 8)) (toPlay pos))
    && not (isInCheck (movePiece pos (Square 5 8) (Square 7 8)) (toPlay pos))
willNotPassCheck pos (Square 5 8) (Square 1 8) =
  not (isInCheck (movePiece pos (Square 5 8) (Square 4 8)) (toPlay pos))
    && not (isInCheck (movePiece pos (Square 5 8) (Square 3 8)) (toPlay pos))
willNotPassCheck _ s1 s2 =
  error
    $ "cannot use squares "
    <> show s1
    <> " and "
    <> show s2
    <> " as castling squares"

insideBoard :: Square -> Bool
insideBoard (Square c r) = c >= 1 && c <= 8 && r >= 1 && r <= 8

insideBoard' :: (Square, Maybe Square) -> Bool
insideBoard' (s, Nothing) = insideBoard s
insideBoard' (s, Just s2) = insideBoard s && insideBoard s2

isInCheck :: Position -> Color -> Bool
isInCheck pos clr =
  let potentialNextPositions =
        positionTreeIgnoreCheckPromotionsCastle pos (succ' clr)
      anyPWithoutKing = anyPosWithoutKing clr potentialNextPositions
   in anyPWithoutKing

isCheckMate :: Position -> Bool
isCheckMate pos = isInCheck pos (toPlay pos) && null (positionTree pos)

isDraw :: Position -> Bool
isDraw pos = isPatt pos || threefoldrepetition pos

threefoldrepetition :: Position -> Bool
threefoldrepetition (Position m' gh _ _) = length (filter (== m') gh) > 1

eqPosition :: Position -> Position -> Bool
eqPosition (Position m1 _ _ _) (Position m2 _ _ _) = m1 == m2

isPatt :: Position -> Bool
isPatt pos = not (isInCheck pos (toPlay pos)) && null (positionTree pos)

anyPosWithoutKing :: Color -> Bunch Position -> Bool
anyPosWithoutKing col pos = not $ allHasKing col pos

allHasKing :: Color -> Bunch Position -> Bool
allHasKing c = all (any (\(_, p) -> p == King c) . colorPieces' c)
  where colorPieces' c' pos' = searchForPieces pos' (const True) (\p -> colr p == c')

determineStatus :: Position -> Status
determineStatus pos
  | toPlay pos == White && isCheckMate pos = WhiteIsMate
  | isCheckMate pos = BlackIsMate
  | isDraw pos = Remis
  | toPlay pos == White = WhiteToPlay
  | otherwise = BlackToPlay
