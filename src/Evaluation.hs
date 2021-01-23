{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Evaluation
  ( Evaluated (..),
    evaluate,
    getPosition,
    evaluate',
    pawnAdvancement,
  )
where

import Chess
import Control.DeepSeq
import GHC.Generics (Generic)
import Position
import Prelude hiding (foldr)

data Evaluated
  = Evaluated
      {-# UNPACK #-} !Position
      {-# UNPACK #-} !Float
      !Status
  deriving (Eq, Show, Generic, NFData)

getPosition :: Evaluated -> Position
getPosition (Evaluated p _ _) = p

evaluate' :: Position -> Status -> Evaluated
evaluate' gh status = case status of
  WhiteIsMate -> Evaluated gh (-10000.0) WhiteIsMate
  BlackIsMate -> Evaluated gh 10000.0 BlackIsMate
  Remis -> Evaluated gh 0 Remis
  playOn -> Evaluated gh (evaluate gh) playOn

evaluate :: Position -> Float
evaluate p =
  let mpos = m p
      squares = toList' mpos
      whiteSurplus = countPieces squares
      pawnAdv = pawnAdvancement squares
      develp = development squares
      safeK = safeKing p
   in whiteSurplus + pawnAdv + develp + safeK

safeKing :: Position -> Float
safeKing p
  | (pieceAt p (Square 7 1) == Just (King White))
      && (pieceAt p (Square 6 1) == Just (Rook White)) =
    0.1
  | (pieceAt p (Square 3 1) == Just (King White))
      && (pieceAt p (Square 4 1) == Just (Rook White)) =
    0.1
  | (pieceAt p (Square 7 8) == Just (King Black))
      && (pieceAt p (Square 6 8) == Just (Rook Black)) =
    -0.1
  | (pieceAt p (Square 3 8) == Just (King Black))
      && (pieceAt p (Square 4 8) == Just (Rook Black)) =
    -0.1
  | otherwise = 0.0

development :: [(Square, Maybe Piece)] -> Float
development squares = sum $ fmap scoreOfficerDevelopment squares

scoreOfficerDevelopment :: (Square, Maybe Piece) -> Float
scoreOfficerDevelopment (_, Nothing) = 0.0
scoreOfficerDevelopment (Square _ row, Just (Knight White)) =
  if row == 1
    then 0.0
    else 0.07
scoreOfficerDevelopment (Square _ row, Just (Knight Black)) =
  if row == 8
    then 0.0
    else (-0.07)
scoreOfficerDevelopment (Square _ row, Just (Bishop White)) =
  if row == 1
    then 0.0
    else 0.06
scoreOfficerDevelopment (Square _ row, Just (Bishop Black)) =
  if row == 8
    then 0.0
    else (-0.06)
scoreOfficerDevelopment (Square _ row, Just (Rook White)) =
  if row == 1
    then 0.0
    else 0.05
scoreOfficerDevelopment (Square _ row, Just (Rook Black)) =
  if row == 8
    then 0.0
    else (-0.05)
scoreOfficerDevelopment _ = 0.0

pawnAdvancement :: [(Square, Maybe Piece)] -> Float
pawnAdvancement squares = sum $ fmap pawnPosValue squares

pawnPosValue :: (Square, Maybe Piece) -> Float
pawnPosValue (_, Nothing) = 0.00
pawnPosValue (Square 3 r, Just (Pawn White)) = r' r * 0.06
pawnPosValue (Square 6 r, Just (Pawn White)) = r' r * 0.06
pawnPosValue (Square 4 r, Just (Pawn White)) = r' r * 0.07
pawnPosValue (Square 5 r, Just (Pawn White)) = r' r * 0.07
pawnPosValue (Square _ r, Just (Pawn White)) = r' r * 0.05
pawnPosValue (Square 3 r, Just (Pawn Black)) = (9 - r' r) * (-0.06)
pawnPosValue (Square 6 r, Just (Pawn Black)) = (9 - r' r) * (-0.06)
pawnPosValue (Square 4 r, Just (Pawn Black)) = (9 - r' r) * (-0.07)
pawnPosValue (Square 5 r, Just (Pawn Black)) = (9 - r' r) * (-0.07)
pawnPosValue (Square _ r, Just (Pawn Black)) = (9 - r' r) * (-0.05)
pawnPosValue _ = 0.00

r' :: Int -> Float
r' n = fromIntegral n :: Float

-- (whitepieces, blackpieces)
countPieces :: [(Square, Maybe Piece)] -> Float
countPieces squares = foldl (\acc (_, mp) -> acc + maybe 0 valueOf mp) 0.0 squares

valueOf :: Piece -> Float
valueOf (Pawn c) = (if c == Black then (-1) else 1) * 1.0
valueOf (Knight c) = (if c == Black then (-1) else 1) * 3.0
valueOf (Bishop c) = (if c == Black then (-1) else 1) * 3.0
valueOf (Rook c) = (if c == Black then (-1) else 1) * 5.0
valueOf (Queen c) = (if c == Black then (-1) else 1) * 9.0
valueOf (King c) = (if c == Black then (-1) else 1) * 100.0
