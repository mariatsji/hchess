{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Position where

import Control.Monad.ST
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.STRef
import Data.Word
import GHC.Generics (Generic)
import Tree

data Color = White | Black
  deriving (Eq, Ord, Enum, Show, Generic)

next :: Color -> Color
next White = Black
next Black = White

data Piece
  = Pawn Color
  | Knight Color
  | Bishop Color
  | Rook Color
  | Queen Color
  | King Color
  deriving (Eq, Ord, Show, Generic)

data Square
  = Square
      Int
      Int
  deriving (Eq, Ord, Show, Generic)

type Snapshot = Tree (Maybe Piece)

data CastleStatus = CanCastleBoth | CanCastleA | CanCastleH | CanCastleNone deriving (Eq, Show, Generic)

data Position
  = Position
      { m :: Snapshot,
        gamehistory :: [Snapshot],
        castleStatusWhite :: CastleStatus,
        castleStatusBlack :: CastleStatus,
        whiteKing :: Maybe Square,
        blackKing :: Maybe Square,
        toPlay :: Color
      }
  deriving (Eq, Show, Generic)

data Move = MovedPiece Square Square | Enpassant Square Square | Promotion Square Piece | Castle Square Square deriving (Eq, Show)

mkPosition :: Position -> Snapshot -> CastleStatus -> CastleStatus -> Maybe Square -> Maybe Square -> Position
mkPosition pos snp csW csB whiteKing' blackKing' =
  let newGH = m pos : gamehistory pos
   in pos {m = snp, gamehistory = newGH, castleStatusWhite = csW, castleStatusBlack = csB, whiteKing = whiteKing', blackKing = blackKing', toPlay = next (toPlay pos)}

mkPositionExpensive :: Position -> Snapshot -> Position
mkPositionExpensive pos@(Position snpa _ csw csb wk bk _) snpb = case findMove snpa snpb of
  MovedPiece from to -> case snpa ?! hash from of
    Just (King White) ->
      mkPosition pos snpb CanCastleNone csb (Just to) bk
    Just (King Black) ->
      mkPosition pos snpb csw CanCastleNone wk (Just to)
    _
      | from == Square 1 1 && csw == CanCastleBoth -> mkPosition pos snpb CanCastleH csb wk bk
      | from == Square 1 1 && csw == CanCastleA -> mkPosition pos snpb CanCastleNone csb wk bk
      | from == Square 8 1 && csw == CanCastleBoth -> mkPosition pos snpb CanCastleA csb wk bk
      | from == Square 8 1 && csw == CanCastleH -> mkPosition pos snpb CanCastleNone csb wk bk
      | from == Square 1 8 && csb == CanCastleBoth -> mkPosition pos snpb csw CanCastleH wk bk
      | from == Square 1 8 && csb == CanCastleA -> mkPosition pos snpb csw CanCastleNone wk bk
      | from == Square 8 8 && csb == CanCastleBoth -> mkPosition pos snpb csw CanCastleA wk bk
      | from == Square 8 8 && csb == CanCastleH -> mkPosition pos snpb csw CanCastleNone wk bk
      | otherwise -> mkPosition pos snpb csw csb wk bk
  Enpassant _ _ -> mkPosition pos snpb csw csb wk bk
  Promotion _ _ -> mkPosition pos snpb csw csb wk bk
  Castle _ to
    | to == Square 1 1 -> mkPosition pos snpb CanCastleNone csb (Just $ Square 3 1) bk
    | to == Square 8 1 -> mkPosition pos snpb CanCastleNone csb (Just $ Square 7 1) bk
    | to == Square 1 8 -> mkPosition pos snpb csw CanCastleNone wk (Just $ Square 3 8)
    | to == Square 8 8 -> mkPosition pos snpb csw CanCastleNone wk (Just $ Square 7 8)
    | otherwise -> error "identified as castle, but does not make any sense"

hash :: Square -> Word8
hash (Square col row) = (fromIntegral row - 1) * 8 + (fromIntegral col - 1)

unHash :: Word8 -> Square
unHash i = Square ((fromIntegral i `rem` 8) + 1) ((fromIntegral i `quot` 8) + 1)

colr :: Piece -> Color
colr (Pawn c) = c
colr (Knight c) = c
colr (Bishop c) = c
colr (Rook c) = c
colr (Queen c) = c
colr (King c) = c

startPosition :: Position
startPosition = Position
  { m = startTree,
    gamehistory = [],
    castleStatusWhite = CanCastleBoth,
    castleStatusBlack = CanCastleBoth,
    whiteKing = Just (Square 5 1),
    blackKing = Just (Square 5 8),
    toPlay = White
  }

startTree :: Snapshot
startTree = fromList' startSquarePieces

startSquarePieces :: [(Square, Piece)]
startSquarePieces = startWhitePieces <> startBlackPieces

startWhitePieces :: [(Square, Piece)]
startWhitePieces =
  [ (Square 1 1, Rook White),
    (Square 2 1, Knight White),
    (Square 3 1, Bishop White),
    (Square 4 1, Queen White),
    (Square 5 1, King White),
    (Square 6 1, Bishop White),
    (Square 7 1, Knight White),
    (Square 8 1, Rook White),
    (Square 1 2, Pawn White),
    (Square 2 2, Pawn White),
    (Square 3 2, Pawn White),
    (Square 4 2, Pawn White),
    (Square 5 2, Pawn White),
    (Square 6 2, Pawn White),
    (Square 7 2, Pawn White),
    (Square 8 2, Pawn White)
  ]

startBlackPieces :: [(Square, Piece)]
startBlackPieces =
  [ (Square 1 7, Pawn Black),
    (Square 2 7, Pawn Black),
    (Square 3 7, Pawn Black),
    (Square 4 7, Pawn Black),
    (Square 5 7, Pawn Black),
    (Square 6 7, Pawn Black),
    (Square 7 7, Pawn Black),
    (Square 8 7, Pawn Black),
    (Square 1 8, Rook Black),
    (Square 2 8, Knight Black),
    (Square 3 8, Bishop Black),
    (Square 4 8, Queen Black),
    (Square 5 8, King Black),
    (Square 6 8, Bishop Black),
    (Square 7 8, Knight Black),
    (Square 8 8, Rook Black)
  ]

movePiece' :: Snapshot -> Square -> Square -> Snapshot
movePiece' snp from to = case snp ?! hash from of
  Nothing ->
    error $ "should be a piece at " <> show from <> " in pos " <> show snp
  (Just piece) -> runST $ do
    pRef <- newSTRef (removePieceAt snp from)
    without <- readSTRef pRef
    writeSTRef pRef (replacePieceAt without to piece)
    readSTRef pRef

removePieceAt :: Snapshot -> Square -> Snapshot
removePieceAt snp s = set snp (hash s) Nothing

replacePieceAt :: Snapshot -> Square -> Piece -> Snapshot
replacePieceAt snp square piece = set snp (hash square) (pure piece)

pieceAt' :: Snapshot -> Square -> Maybe Piece
pieceAt' snp s = snp ?! hash s

searchForPieces :: Position -> (Square -> Bool) -> (Piece -> Bool) -> [(Square, Piece)]
searchForPieces pos squarePred piecePred = catSndMaybes $ unHash <$.> searchIdx (m pos) (squarePred . unHash) (maybe False piecePred)

fromList' :: [(Square, Piece)] -> Snapshot
fromList' = foldr
  (\(s, p) tree -> set tree (hash s) (pure p))
  (empty64 Nothing)

toList' :: Snapshot -> [(Square, Maybe Piece)]
toList' snp = unHash <$.> searchIdx snp (const True) (const True)

catSndMaybes :: [(a, Maybe b)] -> [(a, b)]
catSndMaybes l =
  l
    >>= ( \case
            (a, Just s) -> [(a, s)]
            _ -> []
        )

(<$.>) :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
(<$.>) = fmap . first

infixl 9 <$.>

emptyBoard :: Position
emptyBoard = Position (empty64 Nothing) [] CanCastleBoth CanCastleBoth Nothing Nothing White

findMove :: Snapshot -> Snapshot -> Move
findMove a b =
  let changedSquaresAndPiece = (\t -> (unHash (fst t), snd t)) <$> a `diff` b
      changedSquares = fst <$> changedSquaresAndPiece
   in case length changedSquares of
        4
          | Square 8 1 `elem` changedSquares -> Castle (Square 5 1) (Square 8 1)
          | Square 1 1 `elem` changedSquares -> Castle (Square 5 1) (Square 1 1)
          | Square 8 8 `elem` changedSquares -> Castle (Square 5 8) (Square 8 8)
          | Square 1 8 `elem` changedSquares -> Castle (Square 5 8) (Square 1 8)
          | otherwise -> error "could not determine position diff of length 4 that does not seem to be a castle"
        3 -> Enpassant (epfromSquare changedSquaresAndPiece) (eptoSquare changedSquaresAndPiece)
        2
          | pawnMovedIn changedSquaresAndPiece a b -> Promotion (promfromSquare changedSquaresAndPiece) (promtoSquare changedSquaresAndPiece)
          | otherwise -> MovedPiece (findFrom b changedSquares) (findTo b changedSquares)
        _ -> error "could not determine changed position when diff length not 2,3,4"

findFrom :: Snapshot -> [Square] -> Square
findFrom _ [] = error "Could not find from square in snapshot"
findFrom snp (s : xs) = if isNothing $ snp ?! hash s then s else findFrom snp xs

findTo :: Snapshot -> [Square] -> Square
findTo _ [] = error "Could not find to square in snapshot"
findTo snp (s : xs) = if isJust $ snp ?! hash s then s else findTo snp xs

epfromSquare :: [(Square, Maybe Piece)] -> Square
epfromSquare l =
  let Square r c = eptoSquare l
   in maybe (error "cant find epfromsquare") fst (listToMaybe (filter (\(Square r' c', _) -> r /= r' && c /= c') l))

eptoSquare :: [(Square, Maybe Piece)] -> Square
eptoSquare l = maybe (error "cant find eptosquare") fst (listToMaybe (filter (\(_, mp) -> isJust mp) l))

pawnMovedIn :: [(Square, Maybe Piece)] -> Snapshot -> Snapshot -> Bool
pawnMovedIn [] _ _ = False
pawnMovedIn ((s@(Square _ r), mp) : xs) from to
  | r == 7 && isNothing mp = from ?! hash s == Just (Pawn White)
  | r == 2 && isNothing mp = from ?! hash s == Just (Pawn Black)
  | otherwise = pawnMovedIn xs from to

promfromSquare :: [(Square, Maybe Piece)] -> Square
promfromSquare [] = error "could not determine promfromSquare"
promfromSquare ((Square c r, _) : xs)
  | r == 2 = Square c r
  | r == 7 = Square c r
  | otherwise = promfromSquare xs

promtoSquare :: [(Square, Maybe Piece)] -> Piece
promtoSquare [] = error "could not determine promtoSquare"
promtoSquare ((Square _ r, mp) : xs)
  | r == 1 = fromMaybe (error "expected white officer in promtosquare") mp
  | r == 8 = fromMaybe (error "expected black officer in promtosquare") mp
  | otherwise = promtoSquare xs
