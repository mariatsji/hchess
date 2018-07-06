module Evaluation(Evaluated, evaluate, evaluate', evaluate'', first, pawnAdvancement, toGH) where

import qualified Data.Map.Lazy as Map

import           Chess

type Evaluated = (GameHistory, Float, Status)

evaluate' :: GameHistory -> Evaluated
evaluate' gh = (gh, evaluateGH gh, determineStatus gh)

evaluate'' :: [Position] -> GameHistory -> [Evaluated]
evaluate'' poses gh = fmap (\p -> evaluate' (p : gh)) poses

toGH :: Evaluated -> (Status, GameHistory)
toGH e = let gh = (\(x,_,_) -> x) e in (determineStatus gh, gh)

evaluate :: Position -> Float
evaluate p = whitePieces' p - blackPieces' p + pawnAdvancement p + development p + safeKing p

evaluateGH :: GameHistory -> Float
evaluateGH gh
  | isCheckMate gh && (toPlay gh == White) = -10000.0
  | isCheckMate gh && (toPlay gh == Black) = 10000.0
  | otherwise = evaluate (head gh)

safeKing :: Position -> Float
safeKing p
  | (pieceAt p ('g', 1) == Just (King White)) && (pieceAt p ('f', 1) == Just (Rook White)) = 0.1
  | (pieceAt p ('c', 1) == Just (King White)) && (pieceAt p ('d', 1) == Just (Rook White)) = 0.1
  | (pieceAt p ('g', 8) == Just (King Black)) && (pieceAt p ('f', 8) == Just (Rook Black)) = -0.1
  | (pieceAt p ('c', 8) == Just (King Black)) && (pieceAt p ('d', 8) == Just (Rook Black)) = -0.1
  | otherwise = 0.0

development :: Position -> Float
development (Position p) = sum $ fmap scoreOfficerDevelopment (Map.toList p)

scoreOfficerDevelopment :: (Square, Maybe Piece) -> Float
scoreOfficerDevelopment ((_, row), Just (Knight White)) = if row == 1 then 0.0 else 0.07
scoreOfficerDevelopment ((_, row), Just (Knight Black)) = if row == 8 then 0.0 else (-0.07)
scoreOfficerDevelopment ((_, row), Just (Bishop White)) = if row == 1 then 0.0 else 0.06
scoreOfficerDevelopment ((_, row), Just (Bishop Black)) = if row == 8 then 0.0 else (-0.06)
scoreOfficerDevelopment ((_, row), Just (Rook White)) = if row == 1 then 0.0 else 0.05
scoreOfficerDevelopment ((_, row), Just (Rook Black)) = if row == 8 then 0.0 else (-0.05)
scoreOfficerDevelopment _ = 0.0

pawnAdvancement :: Position -> Float
pawnAdvancement (Position pos) = sum $ fmap pawnPosValue (Map.toList pos)

pawnPosValue :: (Square, Maybe Piece) -> Float
pawnPosValue (('c', r), Just (Pawn White)) = r' r * 0.06
pawnPosValue (('f', r), Just (Pawn White)) = r' r * 0.06
pawnPosValue (('d', r), Just (Pawn White)) = r' r * 0.07
pawnPosValue (('e', r), Just (Pawn White)) = r' r * 0.07
pawnPosValue ((_, r), Just (Pawn White))   = r' r * 0.05
pawnPosValue (('c', r), Just (Pawn Black)) = (9 - r' r) * (-0.06)
pawnPosValue (('f', r), Just (Pawn Black)) = (9 - r' r) * (-0.06)
pawnPosValue (('d', r), Just (Pawn Black)) = (9 - r' r) * (-0.07)
pawnPosValue (('e', r), Just (Pawn Black)) = (9 - r' r) * (-0.07)
pawnPosValue ((_, r), Just (Pawn Black))   =   (9 - r' r) * (-0.05)
pawnPosValue _                             = 0.00

r' :: Int -> Float
r' n = fromIntegral n :: Float

whitePieces' :: Position -> Float
whitePieces' pos = count pos Chess.whitePieces

blackPieces' :: Position -> Float
blackPieces' pos = count pos Chess.blackPieces

count :: Position -> (Position -> [(Square, Piece)]) -> Float
count pos f = foldl (\a (_, p) -> a + valueOf p) 0.0 (f pos)

valueOf :: Piece -> Float
valueOf (Pawn White)   = 1.0
valueOf (Pawn Black)   = 1.0
valueOf (Knight White) = 3.0
valueOf (Knight Black) = 3.0
valueOf (Bishop White) = 3.0
valueOf (Bishop Black) = 3.0
valueOf (Rook White)   = 5.0
valueOf (Rook Black)   = 5.0
valueOf (Queen White)  = 9.0
valueOf (Queen Black)  = 9.0
valueOf (King White)   = 100.0
valueOf (King Black)   = 100.0

first :: GameHistory -> GameHistory
first gh = head (Chess.positionTree gh) : gh
