{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}

module Evaluation
  ( Evaluated (..),
    evaluate,
    getPosition,
    evaluate',
    evaluate'',
    pawnAdvancement,
    toGH
    )
where

import Bunch
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

evaluate' :: Position -> Evaluated
evaluate' gh = Evaluated gh (evaluateGH gh) (determineStatus gh)

evaluate'' :: Bunch Position -> Bunch Evaluated
evaluate'' = fmap evaluate'

toGH :: Evaluated -> (Status, Position)
toGH e =
  let gh = (\(Evaluated x _ _) -> x) e
   in (determineStatus gh, gh)

evaluate :: Position -> Float
evaluate p =
  let whiteSurplus = countPieces p
      pawnAdv = pawnAdvancement p
      develp = development p
      safeK = safeKing p
   in whiteSurplus + pawnAdv + develp + safeK

evaluateGH :: Position -> Float
evaluateGH gh
  | isCheckMate gh && (toPlay gh == White) = -10000.0
  | isCheckMate gh && (toPlay gh == Black) = 10000.0
  | otherwise = evaluate gh

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

development :: Position -> Float
development pos = sum $ fmap scoreOfficerDevelopment (toList' (m pos))

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

pawnAdvancement :: Position -> Float
pawnAdvancement pos = sum $ fmap pawnPosValue (toList' (m pos))

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
countPieces :: Position -> Float
countPieces pos = foldl (\acc (_,mp) -> acc + maybe 0 valueOf mp) 0.0 (toList' (m pos))

valueOf :: Piece -> Float
valueOf (Pawn c) = (if c == Black then (-1) else 1) * 1.0
valueOf (Knight c) = (if c == Black then (-1) else 1) * 3.0
valueOf (Bishop c) = (if c == Black then (-1) else 1) * 3.0
valueOf (Rook c) = (if c == Black then (-1) else 1) * 5.0
valueOf (Queen c) = (if c == Black then (-1) else 1) * 9.0
valueOf (King c) = (if c == Black then (-1) else 1) * 100.0
