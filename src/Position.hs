{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Position where

import Bunch
import Control.DeepSeq (NFData)
import Control.Monad.ST
import Data.List (partition)
import Data.STRef
import Data.Word
import GHC.Generics (Generic)

type BitMap = [Word8]

pieceHash :: Piece -> Word8
pieceHash (Pawn White) = 1
pieceHash (Pawn Black) = 33
pieceHash (Knight White) = 2
pieceHash (Knight Black) = 34
pieceHash (Bishop White) = 3
pieceHash (Bishop Black) = 35
pieceHash (Rook White) = 4
pieceHash (Rook Black) = 36
pieceHash (Queen White) = 5
pieceHash (Queen Black) = 37
pieceHash (King White) = 6
pieceHash (King Black) = 38

pieceUnhash :: Word8 -> Maybe Piece
pieceUnhash 1 = Just $ Pawn White
pieceUnhash 33 = Just $ Pawn Black
pieceUnhash 2 = Just $ Knight White
pieceUnhash 34 = Just $ Knight Black
pieceUnhash 3 = Just $ Bishop White
pieceUnhash 35 = Just $ Bishop Black
pieceUnhash 4 = Just $ Rook White
pieceUnhash 36 = Just $ Rook Black
pieceUnhash 5 = Just $ Queen White
pieceUnhash 37 = Just $ Queen Black
pieceUnhash 6 = Just $ King White
pieceUnhash 38 = Just $ King Black
pieceUnhash 0 = Nothing
pieceUnhash i = error $ "Could not decode piece " <> show i

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

type Snapshot = BitMap

data Position
  = Position
      { m :: !Snapshot,
        gamehistory :: [Snapshot]
        }
  deriving (Eq, Show, Generic, NFData)

mkPosition :: Position -> Snapshot -> Position
mkPosition pos snp =
  let newGH = m pos : gamehistory pos
   in pos {m = snp, gamehistory = newGH}

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
  { m =
      fromList' startSquarePieces,
    gamehistory = []
    }

fromList' :: [(Square, Piece)] -> BitMap
fromList' list =
  foldr
    ( \i acc ->
        let maybePiece = lookup (unHash i) list
         in maybe 0 pieceHash maybePiece : acc
      )
    []
    [0 .. 63]
    :: [Word8]

initLookupPos :: BitMap -> Word8 -> Word8
initLookupPos bitmap i =
  let maybePiece
        | i < 16 = lookup' bitmap (unHash i)
        | i < 48 = Nothing
        | otherwise = lookup' bitmap (unHash i)
   in maybe 0 pieceHash maybePiece

setIn :: BitMap -> Word8 -> Word8 -> BitMap
setIn array pos val =
  foldr (\(idx, encoded) acc -> (if idx == pos then val else encoded) : acc) [] $ zip [0 :: Word8 ..] array

lookup' :: BitMap -> Square -> Maybe Piece
lookup' bitmap s = pieceUnhash $ bitmap !! fromIntegral (hash s)

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

emptyBoard :: Position
emptyBoard = Position {m = replicate 64 0 :: [Word8], gamehistory = []}

movePiece' :: Snapshot -> Square -> Square -> Snapshot
movePiece' snp from to = case lookup' snp from of
  Nothing ->
    error $ "should be a piece at " <> show from <> " in pos " <> show snp
  (Just piece) -> runST $ do
    pRef <- newSTRef (removePieceAt snp from)
    without <- readSTRef pRef
    writeSTRef pRef (replacePieceAt without to piece)
    readSTRef pRef

removePieceAt :: BitMap -> Square -> BitMap
removePieceAt bitmap s = setIn bitmap (hash s) 0

replacePieceAt :: BitMap -> Square -> Piece -> BitMap
replacePieceAt bitmap square piece = setIn bitmap (hash square) (pieceHash piece)

pieceAt' :: Snapshot -> Square -> Maybe Piece
pieceAt' = lookup'

searchForPieces :: Position -> (Piece -> Bool) -> Bunch (Square, Piece)
searchForPieces pos searchpred =
  let sp = asList' (m pos)
   in Bunch $ filter (searchpred . snd) sp
{-# INLINE searchForPieces #-}

-- (whitepieces, blackpieces)
partitionPieces :: Position -> ([Piece], [Piece])
partitionPieces pos = partition (\p -> colr p == White) (snd <$> asList' (m pos))

asList' :: BitMap -> [(Square, Piece)]
asList' bitmap =
  zip [0 ..] bitmap
    >>= ( \(i, p) ->
            case pieceUnhash p of
              Nothing -> []
              Just p' -> [(unHash i, p')]
          )

asListWithEmpties' :: BitMap -> [(Square, Maybe Piece)]
asListWithEmpties' bitmap =
  zip [0 ..] bitmap
    >>= ( \(i, p) ->
            case pieceUnhash p of
              Nothing -> [(unHash i, Nothing)]
              Just p' -> [(unHash i, pure p')]
          )

fmapFirst :: (a -> b) -> (a, c) -> (b, c)
fmapFirst f (a, c) = (f a, c)
{-# INLINE fmapFirst #-}

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx val = max mn (min val mx)
{-# INLINE clamp #-}
