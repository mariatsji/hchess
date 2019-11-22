{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Position where

import Bunch
import Control.DeepSeq (NFData)
import Data.Maybe (fromMaybe)
import Control.Monad.ST
import Data.STRef
import Data.Word
import GHC.Generics (Generic)
import Tree

data Color = White | Black
  deriving (Eq, Ord, Enum, Show, Generic, NFData)

data Piece
  = Pawn !Color
  | Knight !Color
  | Bishop !Color
  | Rook !Color
  | Queen !Color
  | King !Color
  deriving (Eq, Ord, Show, Generic, NFData)

data Square
  = Square
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  deriving (Eq, Ord, Show, Generic, NFData)

type Snapshot = Tree (Maybe Piece)

data CastleStatus = CanCastleBoth | CanCastleA | CanCastleH | CanCastleNone deriving (Eq, Show, Generic, NFData)

data Position
  = Position
      { m :: !Snapshot,
        gamehistory :: [Snapshot],
        castleStatusWhite :: CastleStatus,
        castleStatusBlack :: CastleStatus,
        whiteKing :: Maybe Square,
        blackKing :: Maybe Square
      }
  deriving (Eq, Show, Generic, NFData)

mkPosition :: Position -> Snapshot -> CastleStatus -> CastleStatus -> Maybe Square -> Maybe Square -> Position
mkPosition pos snp csW csB whiteKing' blackKing' =
  let newGH = m pos : gamehistory pos
   in pos {m = snp, gamehistory = newGH, castleStatusWhite = csW, castleStatusBlack = csB, whiteKing = whiteKing', blackKing = blackKing'}

mkPositionExpensive :: Position -> Snapshot -> Position
mkPositionExpensive pos snp = undefined --TODO

hash :: Square -> Word8
hash (Square col row) = (fromIntegral row - 1) * 8 + (fromIntegral col - 1)
{-# INLINE hash #-}

unHash :: Word8 -> Square
unHash i = Square ((fromIntegral i `rem` 8) + 1) ((fromIntegral i `quot` 8) + 1)
{-# INLINE unHash #-}

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
    blackKing = Just (Square 5 8)
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

searchForPieces :: Position -> (Square -> Bool) -> (Piece -> Bool) -> Bunch (Square, Piece)
searchForPieces pos squarePred piecePred = Bunch $ catSndMaybes $ unHash <$.> searchIdx (m pos) (squarePred . unHash) (maybe False piecePred)

fromList' :: [(Square, Piece)] -> Snapshot
fromList' = foldl (\tree (s, p) -> set tree (hash s) (pure p)) (empty64 Nothing)

toList' :: Snapshot -> [(Square, Maybe Piece)]
toList' snp = unHash <$.> searchIdx snp (const True) (const True)

catSndMaybes :: [(a, Maybe b)] -> [(a, b)]
catSndMaybes l =
  l
    >>= ( \case
            (a, Just s) -> [(a, s)]
            _ -> []
        )
{-# INLINE catSndMaybes #-}

(<$.>) :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
(<$.>) f = fmap (\(a', c') -> (f a', c'))

infixl 9 <$.>

{-# INLINE (<$.>) #-}

emptyBoard :: Position
emptyBoard = Position (empty64 Nothing) [] CanCastleBoth CanCastleBoth Nothing Nothing

data Move = MovedPiece Square Square | Enpassant Square Square | Promotion Square Piece | Castle Square Square deriving (Eq, Show)

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
        2 | twoPiecesOfSameColor changedSquaresAndPiece -> Promotion (promfromSquare changedSquaresAndPiece) (promtoSquare changedSquaresAndPiece)
        2 -> MovedPiece (changedSquares !! 1) (head changedSquares)
        _ -> error "could not determine changed position when diff length not 2,3,4"

epfromSquare :: [(Square, Maybe Piece)] -> Square
epfromSquare [] = error "could not determine epfromSquare"
epfromSquare ((Square c r, mp) : xs)
  | r == 4 = Square c r
  | r == 6 = Square c r
  | otherwise = epfromSquare xs

eptoSquare :: [(Square, Maybe Piece)] -> Square
eptoSquare [] = error "could not determine eptoSquare"
eptoSquare ((Square c r, mp) : xs)
  | r == 3 = Square c r
  | r == 7 = Square c r
  | otherwise = eptoSquare xs

twoPiecesOfSameColor :: [(Square, Maybe Piece)] -> Bool
twoPiecesOfSameColor l
  | length l == 2 =
    let colors = fmap (\(_, mp) -> colr <$> mp) l
     in colors == [Just White, Just White] || colors == [Just Black, Just Black]
  | otherwise = error "could not check two pieces of same color since list length /= 2"

promfromSquare :: [(Square, Maybe Piece)] -> Square
promfromSquare [] = error "could not determine promfromSquare"
promfromSquare ((Square c r, mp) : xs)
  | r == 2 = Square c r
  | r == 7 = Square c r
  | otherwise = promfromSquare xs

promtoSquare :: [(Square, Maybe Piece)] -> Piece
promtoSquare [] = error "could not determine promtoSquare"
promtoSquare ((Square c r, mp) : xs)
  | r == 1 = fromMaybe (error "expected white officer in promtosquare") mp
  | r == 8 = fromMaybe (error "expected black officer in promtosquare") mp
  | otherwise = promtoSquare xs