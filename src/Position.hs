{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Position where

import Bunch
import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
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

type Snapshot = IntMap Piece

data Position
  = Position
      { m :: !Snapshot,
        gamehistory :: [Snapshot]
        }
  deriving (Eq, Show, Generic, NFData)

hash :: Square -> Map.Key
hash (Square col row) = (row - 1) * 8 + (col - 1)
{-# INLINE hash #-}

unHash :: Map.Key -> Square
unHash i = Square ((i `rem` 8) + 1) ((i `quot` 8) + 1)
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
  { m =
      fromList' $ Bunch 
         [(Square 1 1, Rook White),
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
movePiece' snp from to = case Map.lookup (hash from) snp of
  Nothing ->
    error $ "should be a piece at " <> show from <> " in pos " <> show snp
  (Just piece) -> runST $ do
    pRef <- newSTRef (removePieceAt snp from)
    without <- readSTRef pRef
    writeSTRef pRef (replacePieceAt without to piece)
    readSTRef pRef

removePieceAt :: Snapshot -> Square -> Snapshot
removePieceAt snp square = Map.delete (hash square) snp

replacePieceAt :: Snapshot -> Square -> Piece -> Snapshot
replacePieceAt snp square piece = Map.insert (hash square) piece snp

pieceAt' :: Snapshot -> Square -> Maybe Piece
pieceAt' sna squ = sna Map.!? hash squ

searchForPieces :: Position -> (Piece -> Bool) -> Bunch (Square, Piece)
searchForPieces pos searchpred = 
  let map' = Map.filterWithKey (const searchpred) (m pos)
  in toList' map'
{-# INLINE searchForPieces #-}

-- promote one position
promoteTo :: Color -> Position -> Piece -> Position
promoteTo c pos@(Position m' _) p = 
  let asList = toList' m'
      asListPromoted = fmap (prom c p) asList
      asMapAgain = fromList' asListPromoted
  in pos {m = asMapAgain}

prom :: Color -> Piece -> (Square, Piece) -> (Square, Piece)
prom White p1 (s@(Square _ r), p2) =
  if r == 8 && p2 == Pawn White then (s, p1) else (s, p2)
prom Black p1 (s@(Square _ r), p2) =
  if r == 1 && p2 == Pawn Black then (s, p1) else (s, p2)

toList' :: IntMap Piece -> Bunch (Square, Piece)
toList' l = Bunch $ first unHash <$> Map.toList l

fromList' :: Bunch (Square, Piece) -> IntMap Piece
fromList' l = Map.fromList $ first hash <$> unBunch l

first :: (a -> b) -> (a, c) -> (b, c)
first f (a,c) = (f a, c)
{-# INLINE first #-}