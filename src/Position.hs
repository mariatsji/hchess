{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Position where

import Bunch
import Control.DeepSeq (NFData)
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
        castleStatusBlack :: CastleStatus
        }
  deriving (Eq, Show, Generic, NFData)

mkPosition :: Position -> Snapshot -> CastleStatus -> CastleStatus -> Position
mkPosition pos snp csW csB =
  let newGH = m pos : gamehistory pos
   in pos {m = snp, gamehistory = newGH, castleStatusWhite = csW, castleStatusBlack = csB}

hash :: Square -> Word8
hash (Square col row) = (fromIntegral row - 1) * 8 + (fromIntegral col - 1)
{-# INLINE hash #-}

unHash :: Word8 -> Square
unHash i = Square ((fromIntegral i `rem` 8) + 1) ((fromIntegral i `quot` 8) + 1) -- TODO Word8 in Square
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
    castleStatusBlack = CanCastleBoth
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
fromList' = foldl (\tree (s,p) -> set tree (hash s) (pure p)) (empty64 Nothing)

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
emptyBoard = Position (empty64 Nothing) [] CanCastleBoth CanCastleBoth