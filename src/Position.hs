{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Position where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import Data.STRef
import Control.Monad.ST
import GHC.Generics (Generic)

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

type Snapshot = Map.Map Square Piece

data Position
  = Position
      { m :: !Snapshot,
        gamehistory :: [Snapshot]
        }
  deriving (Eq, Show, Generic, NFData)

colr :: Piece -> Color
colr (Pawn c) = c
colr (Knight c) = c
colr (Bishop c) = c
colr (Rook c) = c
colr (Queen c) = c
colr (King c) = c

startPosition :: Position
startPosition = Position
  { m =
      Map.fromList
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
          (Square 8 2, Pawn White),
          (Square 1 7, Pawn Black),
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
          ],
    gamehistory = []
    }
  
emptyBoard :: Position
emptyBoard = Position {m = Map.empty, gamehistory = []}

movePiece' :: Snapshot -> Square -> Square -> Snapshot
movePiece' snp from to = case Map.lookup from snp of
  Nothing ->
    error $ "should be a piece at " <> show from <> " in pos " <> show snp
  (Just piece) -> runST $ do
    pRef <- newSTRef (removePieceAt snp from)
    without <- readSTRef pRef
    writeSTRef pRef (replacePieceAt without to piece)
    readSTRef pRef

removePieceAt :: Snapshot -> Square -> Snapshot
removePieceAt snp square = Map.delete square snp

replacePieceAt :: Snapshot -> Square -> Piece -> Snapshot
replacePieceAt snp square piece = Map.insert square piece snp

pieceAt' :: Snapshot -> Square -> Maybe Piece
pieceAt' sna squ = sna Map.!? squ

whitePieces :: Position -> [(Square, Piece)]
whitePieces = Map.foldMapWithKey f . m
  where
    f :: Square -> Piece -> [(Square, Piece)]
    f s p
      | colr p == White = [(s, p)]
      | otherwise = []

blackPieces :: Position -> [(Square, Piece)]
blackPieces = Map.foldMapWithKey f . m
  where
    f :: Square -> Piece -> [(Square, Piece)]
    f s p
      | colr p == Black = [(s, p)]
      | otherwise = []

-- promote one position
promoteTo :: Color -> Position -> Piece -> Position
promoteTo c pos@(Position m' _) p = pos {m = Map.fromList $ fmap (prom c p) (Map.toList m')}

prom :: Color -> Piece -> (Square, Piece) -> (Square, Piece)
prom White p1 (s@(Square _ r), p2) =
  if r == 8 && p2 == Pawn White then (s, p1) else (s, p2)
prom Black p1 (s@(Square _ r), p2) =
  if r == 1 && p2 == Pawn Black then (s, p1) else (s, p2)