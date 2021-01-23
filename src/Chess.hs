{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Chess where

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
movePiece pos@(Position m' _ _ _ wk bk tp) from@(Square fc _) to@(Square tc tr)
  | pieceAt pos from == Just (Pawn White) && (fc /= tc) && vacantAt pos to =
    let newSnapshot =
          movePiece' (removePieceAt m' (Square tc (tr - 1))) from to
     in mkPosition pos newSnapshot (castleStatusWhite pos) (castleStatusBlack pos) wk bk
  | pieceAt pos from == Just (Pawn Black) && (fc /= tc) && vacantAt pos to =
    let newSnapshot =
          movePiece' (removePieceAt m' (Square tc (tr + 1))) from to
     in mkPosition pos newSnapshot (castleStatusWhite pos) (castleStatusBlack pos) wk bk
  | otherwise =
    let newSnapshot = movePiece' m' from to
        newCastleStatusWhite = case tp of
          White -> mkNewCastleStatus pos White from
          Black -> castleStatusWhite pos
        newCastleStatusBlack = case tp of
          Black -> mkNewCastleStatus pos Black from
          White -> castleStatusBlack pos
        newWhiteKing
          | pieceAt pos to == Just (King White) = Nothing
          | pieceAt pos from == Just (King White) = Just to
          | otherwise = wk
        newBlackKing
          | pieceAt pos to == Just (King Black) = Nothing
          | pieceAt pos from == Just (King Black) = Just to
          | otherwise = bk
     in mkPosition pos newSnapshot newCastleStatusWhite newCastleStatusBlack newWhiteKing newBlackKing

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

enemyAt :: Position -> Square -> Square -> Bool
enemyAt pos f t =
  fmap (succ' . colr) (pieceAt pos t) == fmap colr (pieceAt pos f)

succ' :: Color -> Color
succ' White = Black
succ' Black = White

vacantAt :: Position -> Square -> Bool
vacantAt pos t = isNothing $ pieceAt pos t

makeMoves :: Position -> [(Square, Square)] -> Position
makeMoves = foldl (uncurry . movePiece)

-- CAF now? would be nice
pieceAt :: Position -> Square -> Maybe Piece
pieceAt = pieceAt' . m

positionTree :: Position -> [Position]
positionTree pos = positionTreeIgnoreCheck pos >>= (\p -> [p | not (isInCheck p (toPlay pos))])

positionTreeIgnoreCheck :: Position -> [Position]
positionTreeIgnoreCheck !pos =
  let c = toPlay pos
   in (searchForPieces pos (const True) (\p -> colr p == c) >>= positionsPrPiece pos >>= promoteBindFriendly c) <> castle pos

positionsPrPiece :: Position -> (Square, Piece) -> [Position]
positionsPrPiece pos@(Position snp _ _ _ _ _ _) (s, p) = case p of
  Pawn _ ->
    map
      ( \t -> case snd t of
          Nothing -> movePiece pos s (fst t)
          Just s' -> movePiece pos {m = removePieceAt snp s'} s (fst t)
      )
      (filter (canGoThere pos s . fst) $ toSquaresPawn pos s)
  Knight _ ->
    fmap
      (movePiece pos s)
      (filter (finalDestinationNotOccupiedBySelf pos s) $ toSquaresKnight s)
  Bishop _ ->
    fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresBishop s)
  Rook _ ->
    fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresRook s)
  Queen _ ->
    fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresQueen s)
  King _ ->
    fmap (movePiece pos s) (filter (canGoThere pos s) $ toSquaresKing s)

-- pawns - returns new squares, along with an optional capture square (because of en passant)
toSquaresPawn :: Position -> Square -> [(Square, Maybe Square)]
toSquaresPawn pos s@(Square _ r)
  | toPlay pos == White =
    filter insideBoard' $
      [(squareTo s 0 2, Nothing) | r == 2, vacantAt pos $ squareTo s 0 2]
        <> [(squareTo s 0 1, Nothing) | vacantAt pos $ squareTo s 0 1]
        <> [(squareTo s (-1) 1, Nothing) | enemyAt pos s $ squareTo s (-1) 1]
        <> [(squareTo s 1 1, Nothing) | enemyAt pos s $ squareTo s 1 1]
        <> [(squareTo s (-1) 1, Just (squareTo s (-1) 0)) | enPassant pos (squareTo s (-1) 0)]
        <> [(squareTo s 1 1, Just (squareTo s 1 0)) | enPassant pos (squareTo s 1 0)]
  | otherwise =
    filter insideBoard' $
      [(squareTo s 0 (-2), Nothing) | r == 7, vacantAt pos $ squareTo s 0 (-2)]
        <> [(squareTo s 0 (-1), Nothing) | vacantAt pos $ squareTo s 0 (-1)]
        <> [(squareTo s (-1) (-1), Nothing) | enemyAt pos s $ squareTo s (-1) (-1)]
        <> [(squareTo s 1 (-1), Nothing) | enemyAt pos s $ squareTo s 1 (-1)]
        <> [(squareTo s (-1) (-1), Just (squareTo s (-1) 0)) | enPassant pos (squareTo s (-1) 0)]
        <> [(squareTo s 1 (-1), Just (squareTo s 1 0)) | enPassant pos (squareTo s 1 0)]

-- en passant
enPassant :: Position -> Square -> Bool
enPassant (Position _ [] _ _ _ _ _) _ = False
enPassant !pos s@(Square c r)
  | toPlay pos == White =
    (r == 5) && pieceAt pos s == Just (Pawn Black) && jumpedHereJustNow pos s
  | otherwise =
    (r == 4) && pieceAt pos s == Just (Pawn White) && jumpedHereJustNow pos s
  where
    piece = Just $ Pawn (toPlay pos)
    fromSquare = if toPlay pos == White then Square c 7 else Square c 2
    jumpedHereJustNow :: Position -> Square -> Bool
    jumpedHereJustNow _ _ =
      not $
        length (gamehistory pos) < 3
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
  let allPieces = searchForPieces pos (const True) (const True)
      listPromoted = fmap (prom c p) allPieces
   in mkPosition pos (fromList' listPromoted) (castleStatusWhite pos) (castleStatusBlack pos) (whiteKing pos) (blackKing pos)

prom :: Color -> Piece -> (Square, Piece) -> (Square, Piece)
prom White p1 (s@(Square _ r), p2) =
  if r == 8 && p2 == Pawn White then (s, p1) else (s, p2)
prom Black p1 (s@(Square _ r), p2) =
  if r == 1 && p2 == Pawn Black then (s, p1) else (s, p2)

-- optimization, only check for promotions with pending pawns
promoteBindFriendly :: Color -> Position -> [Position]
promoteBindFriendly c pos =
  if Just (Pawn c) `elem` [pieceAt pos (Square col (promoteRow c)) | col <- [1 .. 8]]
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

castle :: Position -> [Position]
castle pos =
  case toPlay pos of
    White -> case castleStatusWhite pos of
      CanCastleNone -> []
      CanCastleBoth -> castleShort pos White <> castleLong pos White
      CanCastleA -> castleLong pos White
      CanCastleH -> castleShort pos White
    Black -> case castleStatusBlack pos of
      CanCastleNone -> []
      CanCastleBoth -> castleShort pos Black <> castleLong pos Black
      CanCastleA -> castleLong pos Black
      CanCastleH -> castleShort pos Black

castleShort :: Position -> Color -> [Position]
castleShort pos color = castle' pos color kingPos shortRookPos doCastleShort

castleLong :: Position -> Color -> [Position]
castleLong pos color = castle' pos color kingPos longRookPos doCastleLong

castle' ::
  Position -> Color -> KingPos -> RookPos -> (Snapshot -> Color -> Snapshot) -> [Position]
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
          newKingPosWhite = case rookPosF color of
            Square 1 1 -> Just $ Square 3 1
            Square 8 1 -> Just $ Square 7 1
            _ -> whiteKing pos
          newKingPosBlack = case rookPosF color of
            Square 1 8 -> Just $ Square 3 8
            Square 8 8 -> Just $ Square 7 8
            _ -> blackKing pos
       in [mkPosition pos newSnapshot newCastleStatusWhite newCastleStatusBlack newKingPosWhite newKingPosBlack]
    else []

doCastleShort :: Snapshot -> Color -> Snapshot
doCastleShort pos c =
  replacePieceAt
    ( replacePieceAt
        (removePieceAt (removePieceAt pos (kingPos c)) (shortRookPos c))
        (Square 7 (homeRow c))
        (King c)
    )
    (Square 6 (homeRow c))
    (Rook c)

doCastleLong :: Snapshot -> Color -> Snapshot
doCastleLong pos c =
  replacePieceAt
    ( replacePieceAt
        (removePieceAt (removePieceAt pos (kingPos c)) (longRookPos c))
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
  not $
    any
      (\p -> isInCheck p (toPlay pos))
      [ pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 1)) (Square 6 1) (King (toPlay pos)), whiteKing = Just (Square 6 1)},
        pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 1)) (Square 7 1) (King (toPlay pos)), whiteKing = Just (Square 7 1)}
      ]
willNotPassCheck pos (Square 5 1) (Square 1 1) =
  not $
    any
      (\p -> isInCheck p (toPlay pos))
      [ pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 1)) (Square 4 1) (King (toPlay pos)), whiteKing = Just (Square 4 1)},
        pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 1)) (Square 3 1) (King (toPlay pos)), whiteKing = Just (Square 3 1)},
        pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 1)) (Square 2 1) (King (toPlay pos)), whiteKing = Just (Square 2 1)}
      ]
willNotPassCheck pos (Square 5 8) (Square 8 8) =
  not $
    any
      (\p -> isInCheck p (toPlay pos))
      [ pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 8)) (Square 6 8) (King (toPlay pos)), blackKing = Just (Square 6 8)},
        pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 8)) (Square 7 8) (King (toPlay pos)), blackKing = Just (Square 7 8)}
      ]
willNotPassCheck pos (Square 5 8) (Square 1 8) =
  not $
    any
      (\p -> isInCheck p (toPlay pos))
      [ pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 8)) (Square 4 8) (King (toPlay pos)), blackKing = Just (Square 4 8)},
        pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 8)) (Square 3 8) (King (toPlay pos)), blackKing = Just (Square 3 8)},
        pos {m = replacePieceAt (removePieceAt (m pos) (Square 5 8)) (Square 2 8) (King (toPlay pos)), blackKing = Just (Square 2 8)}
      ]
willNotPassCheck _ s1 s2 =
  error $
    "cannot use squares "
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
isInCheck pos White =
  case whiteKing pos of
    Nothing -> False
    Just kingSquare ->
      let cangothere = canGoThere pos kingSquare
          checkByPawn = pieceAt pos (squareTo kingSquare (-1) 1) == Just (Pawn Black) || pieceAt pos (squareTo kingSquare 1 1) == Just (Pawn Black)
          checkByKnight = any ((Just (Knight Black) ==) . pieceAt pos) $ filter cangothere (toSquaresKnight kingSquare)
          checkByRookQueen = any ((`elem` [Just (Rook Black), Just (Queen Black)]) . pieceAt pos) $ filter cangothere (toSquaresRook kingSquare)
          checkByBishopQueen = any ((`elem` [Just (Bishop Black), Just (Queen Black)]) . pieceAt pos) $ filter cangothere (toSquaresBishop kingSquare)
          checkByKing = any ((Just (King Black) ==) . pieceAt pos) $ filter cangothere (toSquaresKing kingSquare) -- todo move this check to canGoThere.. case I am a King piece -> check if causes check, else ..
       in checkByPawn || checkByKnight || checkByRookQueen || checkByBishopQueen || checkByKing
isInCheck pos Black =
  case blackKing pos of
    Nothing -> False
    Just kingSquare ->
      let cangothere = canGoThere pos kingSquare
          checkByPawn = pieceAt pos (squareTo kingSquare (-1) (-1)) == Just (Pawn White) || pieceAt pos (squareTo kingSquare 1 (-1)) == Just (Pawn White)
          checkByKnight = any ((Just (Knight White) ==) . pieceAt pos) $ toSquaresKnight kingSquare
          checkByRookQueen = any ((`elem` [Just (Rook White), Just (Queen White)]) . pieceAt pos) $ filter cangothere (toSquaresRook kingSquare)
          checkByBishopQueen = any ((`elem` [Just (Bishop White), Just (Queen White)]) . pieceAt pos) $ filter cangothere (toSquaresBishop kingSquare)
          checkByKing = any ((Just (King White) ==) . pieceAt pos) $ filter cangothere (toSquaresKing kingSquare)
       in checkByPawn || checkByKnight || checkByRookQueen || checkByBishopQueen || checkByKing

isCheckMate :: Position -> [Position] -> Bool
isCheckMate pos positiontree = isInCheck pos (toPlay pos) && null positiontree

isDraw :: Position -> [Position] -> Bool
isDraw pos ptree = isPatt pos ptree || threefoldrepetition pos

threefoldrepetition :: Position -> Bool
threefoldrepetition (Position m' gh _ _ _ _ _) = length gh > 5 && length (filter (== m') gh) > 1

eqPosition :: Position -> Position -> Bool
eqPosition (Position m1 _ _ _ _ _ _) (Position m2 _ _ _ _ _ _) = m1 == m2

isPatt :: Position -> [Position] -> Bool
isPatt pos positiontree = not (isInCheck pos (toPlay pos)) && null positiontree

determineStatus :: Position -> Status
determineStatus pos
  | toPlay pos == White && isCheckMate pos ptree = WhiteIsMate
  | isCheckMate pos ptree = BlackIsMate
  | isDraw pos ptree = Remis
  | toPlay pos == White = WhiteToPlay
  | otherwise = BlackToPlay
  where ptree = positionTree pos