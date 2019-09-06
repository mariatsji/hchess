{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}

module Evaluation
  ( Evaluated (..),
    evaluate,
    evaluate',
    evaluate'',
    pawnAdvancement,
    toGH
    )
where

import Chess
import Control.DeepSeq
import Data.List
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Prelude hiding (foldr)

data Evaluated
  = Evaluated
      {-# UNPACK #-} !Position
      {-# UNPACK #-} !Float
      !Status
  deriving (Eq, Show, Generic, NFData)

evaluate' :: Position -> Evaluated
evaluate' gh = Evaluated gh (evaluateGH gh) (determineStatus gh)

evaluate'' :: [Position] -> [Evaluated]
evaluate'' = map evaluate'

toGH :: Evaluated -> (Status, Position)
toGH e =
  let gh = (\(Evaluated x _ _) -> x) e
   in (determineStatus gh, gh)

evaluate :: Position -> Float
evaluate p =
  let (whiteCount, blackCount) = force $ countPieces p
      pawnAdv = force $ pawnAdvancement p
      develp = force $ development p
      safeK = force $ safeKing p
   in whiteCount - blackCount + pawnAdv + develp + safeK

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
development (Position m' _) = sum $ fmap scoreOfficerDevelopment (Map.toList m')

scoreOfficerDevelopment :: (Square, Piece) -> Float
scoreOfficerDevelopment (Square _ row, Knight White) =
  if row == 1
    then 0.0
    else 0.07
scoreOfficerDevelopment (Square _ row, Knight Black) =
  if row == 8
    then 0.0
    else (-0.07)
scoreOfficerDevelopment (Square _ row, Bishop White) =
  if row == 1
    then 0.0
    else 0.06
scoreOfficerDevelopment (Square _ row, Bishop Black) =
  if row == 8
    then 0.0
    else (-0.06)
scoreOfficerDevelopment (Square _ row, Rook White) =
  if row == 1
    then 0.0
    else 0.05
scoreOfficerDevelopment (Square _ row, Rook Black) =
  if row == 8
    then 0.0
    else (-0.05)
scoreOfficerDevelopment _ = 0.0

pawnAdvancement :: Position -> Float
pawnAdvancement (Position m' _) = sum $ fmap pawnPosValue (Map.toList m')

pawnPosValue :: (Square, Piece) -> Float
pawnPosValue (Square 3 r, Pawn White) = r' r * 0.06
pawnPosValue (Square 6 r, Pawn White) = r' r * 0.06
pawnPosValue (Square 4 r, Pawn White) = r' r * 0.07
pawnPosValue (Square 5 r, Pawn White) = r' r * 0.07
pawnPosValue (Square _ r, Pawn White) = r' r * 0.05
pawnPosValue (Square 3 r, Pawn Black) = (9 - r' r) * (-0.06)
pawnPosValue (Square 6 r, Pawn Black) = (9 - r' r) * (-0.06)
pawnPosValue (Square 4 r, Pawn Black) = (9 - r' r) * (-0.07)
pawnPosValue (Square 5 r, Pawn Black) = (9 - r' r) * (-0.07)
pawnPosValue (Square _ r, Pawn Black) = (9 - r' r) * (-0.05)
pawnPosValue _ = 0.00

r' :: Int -> Float
r' n = fromIntegral n :: Float

-- (whitepieces, blackpieces)
countPieces :: Position -> (Float, Float)
countPieces (Position m' _) =
  let pieces = Map.elems m'
      (whiteList, blackList) = partition (\p -> colr p == White) pieces
   in (sum $ map valueOf whiteList, sum $ map valueOf blackList)

valueOf :: Piece -> Float
valueOf (Pawn White) = 1.0
valueOf (Pawn Black) = 1.0
valueOf (Knight White) = 3.0
valueOf (Knight Black) = 3.0
valueOf (Bishop White) = 3.0
valueOf (Bishop Black) = 3.0
valueOf (Rook White) = 5.0
valueOf (Rook Black) = 5.0
valueOf (Queen White) = 9.0
valueOf (Queen Black) = 9.0
valueOf (King White) = 100.0
valueOf (King Black) = 100.0
