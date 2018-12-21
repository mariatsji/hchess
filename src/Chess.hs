{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Chess
  ( board
  , onlyPiecesBoard
  , Color(..)
  , Piece(..)
  , Square(..)
  , Position(..)
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
  , isDraw
  , succ'
  , promote
  , promoteTo
  , promoteBindFriendly
  , castle
  , castleShort
  , castleLong
  , determineStatus
  )
where

import           Control.DeepSeq
import           Control.Monad.ST
import           Control.Parallel
import           Data.List
import qualified Data.Map.Lazy                 as Map
import           Data.Maybe
import           Data.STRef
import           GHC.Generics                             ( Generic )
import           Prelude                           hiding ( foldl
                                                          , foldl'
                                                          , foldr
                                                          )

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

data Square =
  Square Int
         Int
  deriving (Eq, Ord, Show, Generic, NFData)

type Snapshot = Map.Map Square Piece

data Position = Position
  { m :: Snapshot
  , gamehistory :: [Snapshot]
  } deriving (Eq, Show, Generic, NFData)

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

colr :: Piece -> Color
colr (Pawn   c) = c
colr (Knight c) = c
colr (Bishop c) = c
colr (Rook   c) = c
colr (Queen  c) = c
colr (King   c) = c

king :: Color -> Piece
king White = King White
king Black = King Black

rook :: Color -> Piece
rook White = Rook White
rook Black = Rook Black

squareTo :: Square -> Int -> Int -> Square
squareTo (Square c r) cols rows = Square (c + cols) (r + rows)

startPosition :: Position
startPosition = Position
  { m           = Map.fromList
    $ [ (Square 1 1, Rook White)
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
  , gamehistory = []
  }

emptyBoard :: Position
emptyBoard = Position { m = Map.empty, gamehistory = [] }

movePiece :: Position -> Square -> Square -> Position
movePiece pos@(Position m' gh') from@(Square fc _) to@(Square tc tr)
  | pieceAt pos from == Just (Pawn White) && (fc /= tc) && vacantAt pos to
  = let newSnapshot =
          movePiece' (removePieceAt m' (Square tc (tr - 1))) from to
    in  Position { m = newSnapshot, gamehistory = m' : gh' }
  | pieceAt pos from == Just (Pawn Black) && (fc /= tc) && vacantAt pos to
  = let newSnapshot =
          movePiece' (removePieceAt m' (Square tc (tr + 1))) from to
    in  Position { m = newSnapshot, gamehistory = m' : gh' }
  | otherwise
  = let newSnapshot = movePiece' m' from to
    in  Position { m = newSnapshot, gamehistory = m' : gh' }

movePiece' :: Snapshot -> Square -> Square -> Snapshot
movePiece' snp from to = case Map.lookup from snp of
  Nothing ->
    error $ "should be a piece at " ++ (show from) ++ " in pos " ++ (show snp)
  (Just piece) -> runST $ do
    pRef    <- newSTRef (removePieceAt snp from)
    without <- readSTRef pRef
    writeSTRef pRef (replacePieceAt without to piece)
    readSTRef pRef

points :: Square -> Square -> [Square]
points (Square c1 r1) (Square c2 r2) =
  let toSquare (c, r) = Square c r in toSquare <$> points' (c1, r1) (c2, r2)

points' :: (Int, Int) -> (Int, Int) -> [(Int, Int)] -- all visited squares
points' (c1, r1) (c2, r2)
  | c1 == c2 || r1 == r2 = middle $ (,) <$> to' c1 c2 <*> to' r1 r2
  | otherwise = filter (\(a, b) -> (a, b) /= (c1, r1) && (a, b) /= (c2, r2))
  $ zip (c1 `to'` c2) (r1 `to'` r2)

middle :: [a] -> [a]
middle l = if length l > 1 then (reverse . init . reverse . init) l else []

to' :: Int -> Int -> [Int]
to' a b | a == b    = [a]
        | a > b     = a : to' (a - 1) b
        | otherwise = a : to' (a + 1) b

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

removePieceAt :: Snapshot -> Square -> Snapshot
removePieceAt snp square = Map.delete square snp

replacePieceAt :: Snapshot -> Square -> Piece -> Snapshot
replacePieceAt snp square piece = Map.insert square piece snp

makeMoves :: Position -> [(Square, Square)] -> Position
makeMoves p []       = p
makeMoves p (x : xs) = makeMoves (movePiece p (fst x) (snd x)) xs

pieceAt :: Position -> Square -> Maybe Piece
pieceAt pos s = (m pos) Map.!? s

pieceAt' :: Snapshot -> Square -> Maybe Piece
pieceAt' snp s = snp Map.!? s

whitePieces :: Position -> [(Square, Piece)]
whitePieces (Position m' _) = Map.foldMapWithKey f m'
 where
  f :: Square -> Piece -> [(Square, Piece)]
  f s p | colr p == White = [(s, p)]
        | otherwise       = []

blackPieces :: Position -> [(Square, Piece)]
blackPieces (Position m' _) = Map.foldMapWithKey f m'
 where
  f :: Square -> Piece -> [(Square, Piece)]
  f s p | colr p == Black = [(s, p)]
        | otherwise       = []

whiteToPlay :: Position -> Bool
whiteToPlay (Position _ gh) = even . length $ gh

toPlay :: Position -> Color
toPlay pos = if whiteToPlay pos then White else Black

positionTree :: Position -> [Position]
positionTree pos =
  filter (\p -> not $ isInCheck p (toPlay pos)) $! positionTreeIgnoreCheck pos
  {--
positionTree :: GameHistory -> [Position]
positionTree gh =
  let player = toPlay gh
      notInCheck = not . flip isInCheck player . (: gh)
      completePT = positionTreeIgnoreCheck gh
  in filter notInCheck completePT
--}

positionTreeIgnoreCheck :: Position -> [Position]
positionTreeIgnoreCheck pos
  | whiteToPlay pos
  = let forceRegularMoves =
          force
            $   whitePieces pos
            >>= positionsPrPiece pos
            >>= promoteBindFriendly White
        forceCastle = force $ castle pos
    in  par forceRegularMoves
            (pseq forceCastle (forceRegularMoves ++ forceCastle))
  | otherwise
  = let forceRegularMoves =
          force
            $     blackPieces pos
              >>= positionsPrPiece pos
              >>= promoteBindFriendly Black
        forceCastle = force $ castle pos
    in  par forceRegularMoves
            (pseq forceCastle (forceRegularMoves ++ forceCastle))

positionTreeIgnoreCheckPromotionsCastle :: Position -> Color -> [Position]
positionTreeIgnoreCheckPromotionsCastle pos White =
  whitePieces pos >>= positionsPrPiece pos
positionTreeIgnoreCheckPromotionsCastle pos Black =
  blackPieces pos >>= positionsPrPiece pos

positionsPrPiece :: Position -> (Square, Piece) -> [Position]
positionsPrPiece pos@(Position snp gh) (s, p) = case p of
  (Pawn _) ->
    let potentials = filter (\t -> canGoThere pos s (fst t)) $ toSquaresPawn pos (s, p)
    in  map
        (\t -> case snd t of
                    Nothing -> movePiece pos s (fst t)
                    Just s' -> movePiece pos { m = removePieceAt snp s'} s (fst t))
        potentials
  (Knight _) -> fmap
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

-- pawns - returns new squares, along with an optional capture square (because of en passant)
toSquaresPawn :: Position -> (Square, Piece) -> [(Square, Maybe Square)]
toSquaresPawn pos (s@(Square _ r), p)
  | colr p == White
  = filter insideBoard'
    $  [ (squareTo s 0 2, Nothing) | r == 2, vacantAt pos $ squareTo s 0 2 ]
    ++ [ (squareTo s 0 1, Nothing) | vacantAt pos $ squareTo s 0 1 ]
    ++ [ (squareTo s (-1) 1, Nothing) | enemyAt pos s $ squareTo s (-1) 1 ]
    ++ [ (squareTo s (-1) 1, Just (squareTo s (-1) 0)) | enPassant pos (squareTo s (-1) 0) ]
    ++ [ (squareTo s 1 1, Nothing) | enemyAt pos s $ squareTo s 1 1 ]
    ++ [ (squareTo s 1 1, Just (squareTo s 1 0)) | enPassant pos (squareTo s 1 0) ]
  | otherwise
  = filter insideBoard'
    $  [ (squareTo s 0 (-2), Nothing) | r == 7, vacantAt pos $ squareTo s 0 (-2) ]
    ++ [ (squareTo s 0 (-1), Nothing) | vacantAt pos $ squareTo s 0 (-1) ]
    ++ [ (squareTo s (-1) (-1), Nothing) | enemyAt pos s $ squareTo s (-1) (-1) ]
    ++ [ (squareTo s (-1) (-1), Just (squareTo s (-1) 0)) | enPassant pos (squareTo s (-1) 0) ]
    ++ [ (squareTo s 1 (-1), Nothing) | enemyAt pos s $ squareTo s 1 (-1) ]
    ++ [ (squareTo s 1 (-1), Just (squareTo s 1 0)) | enPassant pos (squareTo s 1 0) ]

-- en passant
enPassant :: Position -> Square -> Bool
enPassant (Position _ []) _ = False
enPassant pos s@(Square c r)
  | toPlay pos == White
  = (r == 5) && pieceAt pos s == Just (Pawn Black) && jumpedHereJustNow pos s
  | otherwise
  = (r == 4) && pieceAt pos s == Just (Pawn White) && jumpedHereJustNow pos s
 where
  piece = if toPlay pos == White then Just (Pawn Black) else Just (Pawn White)
  fromSquare = if toPlay pos == White then Square c 7 else Square c 2
  jumpedHereJustNow :: Position -> Square -> Bool
  jumpedHereJustNow pos' s' =
    if length (gamehistory pos) < 3 then False
    else
      let prevSnapshot = gamehistory pos !! 1
      in pieceAt' prevSnapshot fromSquare == piece && pieceAt' (m pos) s == piece && pieceAt' prevSnapshot s == Nothing
    

prom :: Color -> Piece -> (Square, Piece) -> (Square, Piece)
prom White p1 (s@(Square _ r), p2) =
  if r == 8 && p2 == Pawn White then (s, p1) else (s, p2)
prom Black p1 (s@(Square _ r), p2) =
  if r == 1 && p2 == Pawn Black then (s, p1) else (s, p2)

-- promote one position
promoteTo :: Color -> Position -> Piece -> Position
promoteTo c pos@(Position m' _) p =
  pos { m = Map.fromList $ fmap (prom c p) (Map.toList m') }

-- promote one position to [] or all four positions
maybePromote :: Color -> Position -> Piece -> [Position]
maybePromote c pos p = [ promoteTo c pos p | canPromote c pos p ]
  where canPromote c' pos' p' = promoteTo c' pos' p' /= pos'

promote :: Color -> Position -> [Position]
promote c@White pos =
  let mpQForced = force (maybePromote c pos (Queen White))
      mpRForced = force (maybePromote c pos (Rook White))
      mpBForced = force (maybePromote c pos (Bishop White))
      mpKForced = force (maybePromote c pos (Knight White))
  in  par
        mpQForced
        (par
          mpRForced
          (par
            mpBForced
            (pseq mpKForced (mpQForced ++ mpRForced ++ mpBForced ++ mpKForced))
          )
        )
promote c@Black pos =
  let mpQForced = force (maybePromote c pos (Queen Black))
      mpRForced = force (maybePromote c pos (Rook Black))
      mpBForced = force (maybePromote c pos (Bishop Black))
      mpKForced = force (maybePromote c pos (Knight Black))
  in  par
        mpQForced
        (par
          mpRForced
          (par
            mpBForced
            (pseq mpKForced (mpQForced ++ mpRForced ++ mpBForced ++ mpKForced))
          )
        )

-- optimization, only check for promotions with pending pawns
promoteBindFriendly :: Color -> Position -> [Position]
promoteBindFriendly White pos =
  if elem (Just (Pawn White)) [ pieceAt pos (Square col 8) | col <- [1 .. 8] ]
    then promoteBindFriendly' White pos
    else [pos]
promoteBindFriendly Black pos =
  if elem (Just (Pawn Black)) [ pieceAt pos (Square col 1) | col <- [1 .. 8] ]
    then promoteBindFriendly' Black pos
    else [pos]

-- same pos or all four
promoteBindFriendly' :: Color -> Position -> [Position]
promoteBindFriendly' c pos =
  if promote c pos /= [pos] && promote c pos /= [] then promote c pos else [pos]

-- knights
toSquaresKnight :: Square -> [Square]
toSquaresKnight s = filter
  insideBoard
  [ squareTo s (-1) 2
  , squareTo s (-1) (-2)
  , squareTo s 1    2
  , squareTo s 1    (-2)
  , squareTo s 2    1
  , squareTo s 2    (-1)
  , squareTo s (-2) 1
  , squareTo s (-2) (-1)
  ]

-- bishops
toSquaresBishop :: Square -> [Square]
toSquaresBishop s@(Square c r) =
  let maxDown  = r - 1
      maxUp    = 8 - r
      maxLeft  = c - 1
      maxRight = 8 - c
      a'       = force $ fmap (\x -> squareTo s x x) [1 .. min maxUp maxRight]
      b' = force $ fmap (\x -> squareTo s x (-x)) [1 .. min maxDown maxRight]
      c' = force $ fmap (\x -> squareTo s (-x) (-x)) [1 .. min maxDown maxLeft]
      d'       = force $ fmap (\x -> squareTo s (-x) x) [1 .. min maxLeft maxUp]
  in  par a' (par b' (par c' (pseq d' (a' ++ b' ++ c' ++ d'))))

-- rooks
toSquaresRook :: Square -> [Square]
toSquaresRook s@(Square c r) =
  let maxDown  = 1 - r
      maxUp    = 8 - r
      maxLeft  = 1 - c
      maxRight = 8 - c
      lane     = fmap (\r' -> squareTo s 0 r') [maxDown .. maxUp]
      row      = fmap (\c' -> squareTo s c' 0) [maxLeft .. maxRight]
      laneF    = force lane
      rowF     = force row
  in  par laneF (pseq rowF (laneF ++ rowF))

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

type PerformCastleF = Snapshot -> Color -> Snapshot

castle :: Position -> [Position]
castle pos = castleShort pos (toPlay pos) ++ castleLong pos (toPlay pos)

castleShort :: Position -> Color -> [Position]
castleShort pos color = castle' pos color kingPos shortRookPos doCastleShort

castleLong :: Position -> Color -> [Position]
castleLong pos color = castle' pos color kingPos longRookPos doCastleLong

castle'
  :: Position -> Color -> KingPos -> RookPos -> PerformCastleF -> [Position]
castle' pos color kingPosF rookPosF doCastleF =
  if pieceAt pos (kingPosF color)
       == Just (king color)
       && -- must have a king at home
          pieceAt pos (rookPosF color)
       == Just (rook color)
       && -- must have a rook at home
          vacantBetween pos (kingPosF color) (rookPosF color)
       && -- must be vacant between king and rook
          haveNotMoved pos (king color) (kingPosF color)
       && -- must not have moved king
          haveNotMoved pos (rook color) (rookPosF color)
       && -- must not have moved rook
          (not (isInCheck pos color)
          && -- must not be in check
             willNotPassCheck pos (kingPosF color) (rookPosF color) -- must not move through check
          )
    then 
      let newSnapshot = doCastleF (m pos) color
      in [Position { m = newSnapshot, gamehistory = (m pos) : (gamehistory pos) }]
    else []

doCastleShort :: Snapshot -> Color -> Snapshot
doCastleShort pos White = replacePieceAt
  (replacePieceAt
    (removePieceAt (removePieceAt pos (Square 5 1)) (Square 8 1))
    (Square 7 1)
    (King White)
  )
  (Square 6 1)
  (Rook White)
doCastleShort pos Black = replacePieceAt
  (replacePieceAt
    (removePieceAt (removePieceAt pos (Square 5 8)) (Square 8 8))
    (Square 7 8)
    (King Black)
  )
  (Square 6 8)
  (Rook Black)

doCastleLong :: Snapshot -> Color -> Snapshot
doCastleLong pos White = replacePieceAt
  (replacePieceAt
    (removePieceAt (removePieceAt pos (Square 5 1)) (Square 1 1))
    (Square 3 1)
    (King White)
  )
  (Square 4 1)
  (Rook White)
doCastleLong pos Black = replacePieceAt
  (replacePieceAt
    (removePieceAt (removePieceAt pos (Square 5 8)) (Square 1 8))
    (Square 3 8)
    (King Black)
  )
  (Square 4 8)
  (Rook Black)

vacantBetween :: Position -> Square -> Square -> Bool
vacantBetween pos from to = all (vacantAt pos) $ points from to

kingPos :: Color -> Square
kingPos White = Square 5 1
kingPos Black = Square 5 8

shortRookPos :: Color -> Square
shortRookPos White = Square 8 1
shortRookPos Black = Square 8 8

longRookPos :: Color -> Square
longRookPos White = Square 1 1
longRookPos Black = Square 1 8

haveNotMoved :: Position -> Piece -> Square -> Bool
haveNotMoved pos'@(Position m' gh') p s =
  all (\pos -> pieceAt' pos s == Just p) gh' && pieceAt pos' s == Just p

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
    $  "cannot use squares "
    ++ show s1
    ++ " and "
    ++ show s2
    ++ " as castling squares"

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
  in  anyPWithoutKing

isCheckMate :: Position -> Bool
isCheckMate pos = isInCheck pos (toPlay pos) && null (positionTree pos)

isDraw :: Position -> Bool
isDraw pos = isPatt pos || threefoldrepetition pos

threefoldrepetition :: Position -> Bool
threefoldrepetition (Position m' gh) = length (filter (\p' -> p' == m') gh) > 1

max' :: Ord a => [a] -> a
max' []       = error "no max element in empty list"
max' [x     ] = x
max' (x : xs) = if x > max' xs then x else max' xs

eqPosition :: Position -> Position -> Bool
eqPosition (Position m1 _) (Position m2 _) = m1 == m2

isPatt :: Position -> Bool
isPatt pos = not (isInCheck pos (toPlay pos)) && null (positionTree pos)

anyPosWithoutKing :: Color -> [Position] -> Bool
anyPosWithoutKing col pos = not $ allHasKing col pos

allHasKing :: Color -> [Position] -> Bool
allHasKing White poses =
  all (any (\(_, p) -> p == King White) . whitePieces) poses
allHasKing Black poses =
  all (any (\(_, p) -> p == King Black) . blackPieces) poses

determineStatus :: Position -> Status
determineStatus pos | toPlay pos == White && isCheckMate pos = WhiteIsMate
                    | isCheckMate pos                        = BlackIsMate
                    | isDraw pos                             = Remis
                    | toPlay pos == White                    = WhiteToPlay
                    | otherwise                              = BlackToPlay
