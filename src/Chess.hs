{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Chess
  ( idx
  , idxToSquare
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
  , toSquaresBishop
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

import           Control.DeepSeq
import           Control.Monad.ST
import           Control.Parallel
import           Data.Int            (Int8)
import           Data.List
import           Data.Maybe
import           Data.STRef
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           GHC.Generics        (Generic)
import           Prelude             hiding (foldl, foldl', foldr)

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

pieceNum :: Piece -> Int8
pieceNum (Pawn White)   = 1
pieceNum (Pawn Black)   = 2
pieceNum (Knight White) = 3
pieceNum (Knight Black) = 4
pieceNum (Bishop White) = 5
pieceNum (Bishop Black) = 6
pieceNum (Rook White)   = 7
pieceNum (Rook Black)   = 8
pieceNum (Queen White)  = 9
pieceNum (Queen Black)  = 10
pieceNum (King White)   = 11
pieceNum (King Black)   = 12

numPiece :: Int8 -> Maybe Piece
numPiece 1  = Just (Pawn White)
numPiece 2  = Just (Pawn Black)
numPiece 3  = Just (Knight White)
numPiece 4  = Just (Knight Black)
numPiece 5  = Just (Bishop White)
numPiece 6  = Just (Bishop Black)
numPiece 7  = Just (Rook White)
numPiece 8  = Just (Rook Black)
numPiece 9  = Just (Queen White)
numPiece 10 = Just (Queen Black)
numPiece 11 = Just (King White)
numPiece 12 = Just (King Black)
numPiece _  = Nothing

data Square =
  Square Col
         Row
  deriving (Eq, Ord, Show, Generic, NFData)

type Col = Int

type Row = Int

type Board = VU.Vector Int8

emptyBoard :: Position
emptyBoard = Position $ VU.generate 64 (const 0)

newtype Position = Position
  { board :: Board
  } deriving (Eq, Show, Generic, NFData)

type GameHistory = [Position]

data Status
  = WhiteToPlay
  | BlackToPlay
  | Remis
  | WhiteIsMate
  | BlackIsMate
  deriving (Eq, Ord, Show, Generic, NFData)

colr :: Piece -> Color
colr p =
  if odd (pieceNum p)
    then White
    else Black

idx :: Square -> Int
idx (Square c r) = (c - 1) + (r - 1) * 8

idxToSquare :: Int -> Square
idxToSquare i =
  let c = (i `mod` 8) + 1 -- 0..8
      r = (i `div` 8) + 1
   in Square c r

king :: Color -> Piece
king White = King White
king Black = King Black

rook :: Color -> Piece
rook White = Rook White
rook Black = Rook Black

squareTo :: Square -> Int -> Int -> Square
squareTo (Square c r) cols rows = Square (c + cols) (r + rows)

startPosition :: Position
startPosition =
  Position $
  VU.fromList $
  [ pieceNum (Rook White)
  , pieceNum (Knight White)
  , pieceNum (Bishop White)
  , pieceNum (Queen White)
  , pieceNum (King White)
  , pieceNum (Bishop White)
  , pieceNum (Knight White)
  , pieceNum (Rook White)
  ] ++
  replicate 8 (pieceNum (Pawn White)) ++
  replicate 32 0 ++
  replicate 8 (pieceNum (Pawn Black)) ++
  [ pieceNum (Rook Black)
  , pieceNum (Knight Black)
  , pieceNum (Bishop Black)
  , pieceNum (Queen Black)
  , pieceNum (King Black)
  , pieceNum (Bishop Black)
  , pieceNum (Knight Black)
  , pieceNum (Rook Black)
  ]

movePiece :: Position -> Square -> Square -> Position
movePiece pos from@(Square fc _) to@(Square tc tr)
  | pieceAt pos from == Just (Pawn White) && (fc /= tc) && vacantAt pos to =
    movePiece' (removePieceAt pos (Square tc (tr - 1))) from to
  | pieceAt pos from == Just (Pawn Black) && (fc /= tc) && vacantAt pos to =
    movePiece' (removePieceAt pos (Square tc (tr + 1))) from to
  | otherwise = movePiece' pos from to

movePiece' :: Position -> Square -> Square -> Position --- xxxpensive?! looks so from profiling. Mutable madness?
movePiece' pos from to =
  case pieceAt pos from of
    Nothing -> pos
    (Just piece) ->
      runST $ do
        pRef <- newSTRef (removePieceAt pos from)
        p <- readSTRef pRef
        writeSTRef pRef (replacePieceAt p to piece)
        readSTRef pRef

points :: Square -> Square -> [Square]
points (Square c1 r1) (Square c2 r2) =
  let toSquare (c, r) = Square c r
   in toSquare <$> points' (c1, r1) (c2, r2)

points' :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -- all visited squares
points' (c1, r1) (c2, r2)
  | c1 == c2 || r1 == r2 = middle $ (,) <$> to' c1 c2 <*> to' r1 r2
  | otherwise =
    filter (\(a, b) -> (a, b) /= (c1, r1) && (a, b) /= (c2, r2)) $
    zip (c1 `to'` c2) (r1 `to'` r2)

middle :: [a] -> [a]
middle l =
  if length l > 1
    then (reverse . init . reverse . init) l
    else []

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
  let before = VU.take (idx square) pos
      after = VU.drop (succ $ idx square) pos
      beforei = VU.snoc before 0
   in Position $ VU.concat [beforei, after]

replacePieceAt :: Position -> Square -> Piece -> Position
replacePieceAt (Position pos) square piece =
  let before = VU.take (idx square) pos
      after = VU.drop (succ $ idx square) pos
      newpiece = pieceNum piece
      beforei = VU.snoc before newpiece
   in Position $ VU.concat [beforei, after]

makeMoves :: GameHistory -> [(Square, Square)] -> GameHistory
makeMoves gh []     = gh
makeMoves gh (x:xs) = makeMoves (movePiece (head gh) (fst x) (snd x) : gh) xs

pieceAt :: Position -> Square -> Maybe Piece
pieceAt (Position pos) square = numPiece $ pos VU.! idx square

whitePieces :: Position -> [(Square, Piece)]
whitePieces p@(Position pos) =
  let squares = map idxToSquare [0 .. 63]
      zipped = zip squares (VU.toList pos)
   in (foldl'
         (\a (s, p) ->
            case numPiece p of
              Nothing -> a
              Just p ->
                if odd (pieceNum p)
                  then (s, p) : a
                  else a)
         []
         zipped)

blackPieces :: Position -> [(Square, Piece)]
blackPieces (Position pos) =
  let squares = map idxToSquare [0 .. 63]
      zipped = zip squares (VU.toList pos)
   in (foldl'
         (\a (s, p) ->
            case numPiece p of
              Nothing -> a
              Just p ->
                if even (pieceNum p)
                  then (s, p) : a
                  else a)
         []
         zipped)

whiteToPlay :: GameHistory -> Bool
whiteToPlay = odd . length

toPlay :: GameHistory -> Color
toPlay pos =
  if whiteToPlay pos
    then White
    else Black

positionTree' :: GameHistory -> [GameHistory]
positionTree' gh = map (: gh) $! positionTree gh

positionTree :: GameHistory -> [Position]
positionTree gh =
  filter (\c -> not $ isInCheck (c : gh) (toPlay gh)) $!
  positionTreeIgnoreCheck gh

positionTreeIgnoreCheck :: GameHistory -> [Position]
positionTreeIgnoreCheck gh
  | whiteToPlay gh =
    let forceRegularMoves =
          force $
          whitePieces (head gh) >>= positionsPrPiece gh >>=
          promoteBindFriendly White
        forceCastle = force $ castle gh
     in par
          forceRegularMoves
          (pseq forceCastle (forceRegularMoves ++ forceCastle))
  | otherwise =
    let forceRegularMoves =
          force $
          (blackPieces (head gh) >>= positionsPrPiece gh >>=
           promoteBindFriendly Black)
        forceCastle = force $ castle gh
     in par
          forceRegularMoves
          (pseq forceCastle (forceRegularMoves ++ forceCastle))

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
toSquaresPawn gh (s@(Square _ r), p)
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
    [(squareTo s 0 (-2), Nothing) | r == 7, vacantAt pos $ squareTo s 0 (-2)] ++
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

-- promote one position
promoteTo :: Color -> Position -> Piece -> Position
promoteTo White (Position pos) piece =
  let (unalteredRows, promotionRow) = VU.splitAt (7 * 8) pos
      finalPromotionRow =
        VU.map
          (\n ->
             case numPiece n of
               Just (Pawn White) -> pieceNum piece
               Just x            -> pieceNum x
               Nothing           -> 0)
          promotionRow
   in Position (unalteredRows VU.++ finalPromotionRow)
promoteTo Black (Position pos) piece =
  let (promotionRow, unalteredRows) = VU.splitAt (1 * 8) pos
      finalPromotionRow =
        VU.map
          (\n ->
             case numPiece n of
               Just (Pawn Black) -> pieceNum piece
               Just x            -> pieceNum x
               Nothing           -> 0)
          promotionRow
   in Position (finalPromotionRow VU.++ unalteredRows)

-- promote one position to [] or all four positions
maybePromote :: Color -> Position -> Piece -> [Position]
maybePromote c pos p = [promoteTo c pos p | canPromote c pos p]
  where
    canPromote c' pos' p' = promoteTo c' pos' p' /= pos'

promote :: Color -> Position -> [Position]
promote c@White pos =
  let mpQForced = force (maybePromote c pos (Queen White))
      mpRForced = force (maybePromote c pos (Rook White))
      mpBForced = force (maybePromote c pos (Bishop White))
      mpKForced = force (maybePromote c pos (Knight White))
   in par
        mpQForced
        (par
           mpRForced
           (par
              mpBForced
              (pseq mpKForced (mpQForced ++ mpRForced ++ mpBForced ++ mpKForced))))
promote c@Black pos =
  let mpQForced = force (maybePromote c pos (Queen Black))
      mpRForced = force (maybePromote c pos (Rook Black))
      mpBForced = force (maybePromote c pos (Bishop Black))
      mpKForced = force (maybePromote c pos (Knight Black))
   in par
        mpQForced
        (par
           mpRForced
           (par
              mpBForced
              (pseq mpKForced (mpQForced ++ mpRForced ++ mpBForced ++ mpKForced))))

-- optimization, only check for promotions with pending pawns
promoteBindFriendly :: Color -> Position -> [Position]
promoteBindFriendly White pos =
  if elem (Just (Pawn White)) [pieceAt pos (Square col 8) | col <- [1 .. 8]]
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
toSquaresBishop s@(Square c r) =
  let maxDown = r - 1
      maxUp = 8 - r
      maxLeft = c - 1
      maxRight = 8 - c
      a' = force $ fmap (\x -> squareTo s x x) [1 .. min maxUp maxRight]
      b' = force $ fmap (\x -> squareTo s x (-x)) [1 .. min maxDown maxRight]
      c' = force $ fmap (\x -> squareTo s (-x) (-x)) [1 .. min maxDown maxLeft]
      d' = force $ fmap (\x -> squareTo s (-x) x) [1 .. min maxLeft maxUp]
   in par a' (par b' (par c' (pseq d' (a' ++ b' ++ c' ++ d'))))

-- rooks
toSquaresRook :: Square -> [Square]
toSquaresRook s@(Square c r) =
  let maxDown = 1 - r
      maxUp = 8 - r
      maxLeft = 1 - c
      maxRight = 8 - c
      lane = fmap (\r' -> squareTo s 0 r') [maxDown .. maxUp]
      row = fmap (\c' -> squareTo s c' 0) [maxLeft .. maxRight]
      laneF = force lane
      rowF = force row
   in par laneF (pseq rowF (laneF ++ rowF))

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
  not
    (isInCheck (movePiece (head gh) (Square 5 1) (Square 6 1) : gh) (toPlay gh)) &&
  not
    (isInCheck (movePiece (head gh) (Square 5 1) (Square 7 1) : gh) (toPlay gh))
willNotPassCheck gh (Square 5 1) (Square 1 1) =
  not
    (isInCheck (movePiece (head gh) (Square 5 1) (Square 4 1) : gh) (toPlay gh)) &&
  not
    (isInCheck (movePiece (head gh) (Square 5 1) (Square 3 1) : gh) (toPlay gh))
willNotPassCheck gh (Square 5 8) (Square 8 8) =
  not
    (isInCheck (movePiece (head gh) (Square 5 8) (Square 6 8) : gh) (toPlay gh)) &&
  not
    (isInCheck (movePiece (head gh) (Square 5 8) (Square 7 8) : gh) (toPlay gh))
willNotPassCheck gh (Square 5 8) (Square 1 8) =
  not
    (isInCheck (movePiece (head gh) (Square 5 8) (Square 4 8) : gh) (toPlay gh)) &&
  not
    (isInCheck (movePiece (head gh) (Square 5 8) (Square 3 8) : gh) (toPlay gh))
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
  let potentialNextPositions =
        positionTreeIgnoreCheckPromotionsCastle gh (succ' clr)
      anyPWithoutKing = anyPosWithoutKing clr potentialNextPositions
   in anyPWithoutKing

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
