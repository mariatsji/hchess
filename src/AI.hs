module AI (first, evaluate, firstBest, pawnAdvancement) where

import Chess

first :: GameHistory -> GameHistory
first gh = head (Chess.positionTree gh) : gh

firstBest :: GameHistory -> GameHistory
firstBest gh
  | (toPlay gh) == White = highest tuples : gh
  | otherwise = lowest tuples : gh
    where tuples = fmap (\p -> (p, evaluate p)) (Chess.positionTree gh)

highest :: [(Position, Float)] -> Position
highest t = fst $ foldl (\(p1, f1) (p2, f2) -> if f1 > f2 then (p1, f1) else (p2, f2)) (Chess.emptyBoard, 0.0 :: Float) t

lowest :: [(Position, Float)] -> Position
lowest t = fst $ foldl (\(p1, f1) (p2, f2) -> if f1 < f2 then (p1, f1) else (p2, f2)) (Chess.emptyBoard, 0.0 :: Float) t

evaluate :: Position -> Float
evaluate p = whitePieces' p - blackPieces' p + pawnAdvancement p

pawnAdvancement :: Position -> Float
pawnAdvancement pos = sum $ fmap pawnPosValue pos

pawnPosValue :: (Square, Maybe Piece) -> Float
pawnPosValue (('c', r), Just (Pawn White)) = r' r * 0.06
pawnPosValue (('f', r), Just (Pawn White)) = r' r * 0.06
pawnPosValue (('d', r), Just (Pawn White)) = r' r * 0.07
pawnPosValue (('e', r), Just (Pawn White)) = r' r * 0.07
pawnPosValue ((x, r), Just (Pawn White)) = r' r * 0.05
pawnPosValue (('c', r), Just (Pawn Black)) = (9 - r' r) * (-0.06)
pawnPosValue (('f', r), Just (Pawn Black)) = (9 - r' r) * (-0.06)
pawnPosValue (('d', r), Just (Pawn Black)) = (9 - r' r) * (-0.07)
pawnPosValue (('e', r), Just (Pawn Black)) = (9 - r' r) * (-0.07)
pawnPosValue ((x, r), Just (Pawn Black)) =   (9 - r' r) * (-0.05)
pawnPosValue x = 0.00

r' :: Int -> Float
r' n = fromIntegral n :: Float

whitePieces' :: Position -> Float
whitePieces' pos = count pos Chess.whitePieces

blackPieces' :: Position -> Float
blackPieces' pos = count pos Chess.blackPieces

count :: Position -> (Position -> [(Square, Piece)]) -> Float
count p f = foldl (\a (s, p) -> a + valueOf p) 0.0 (f p)

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