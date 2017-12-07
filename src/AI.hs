module AI (evaluate, first, pawnAdvancement, positionTreeSearch, bestSearchedGH) where

import Chess

-- gives you the full potential gamehistory for the current gamehistory, as given by search depth
bestSearchedGH :: GameHistory -> Evaluated
bestSearchedGH gh
  | toPlay gh == White = highest $ fmap evaluate' $ positionTreeSearch gh
  | otherwise = lowest $ fmap evaluate' $ positionTreeSearch gh

positionTreeSearch :: GameHistory -> [GameHistory]
positionTreeSearch gh = positionTree' gh >>= positionTree' >>= positionTree' >>= positionTree'

-- takes actual gh, and the chosen best path forward as a single position on top of current gh
nextPositionBest :: GameHistory -> GameHistory -> GameHistory
nextPositionBest gh potentialGh = if length potentialGh > length gh then (potentialGh !! (length gh)) : gh else gh

highest :: [Evaluated] -> Evaluated
highest t = foldl1 (\(p1, f1, s1) (p2, f2, s2) -> if f1 > f2 then (p1, f1, s1) else (p2, f2, s2)) t

lowest :: [Evaluated] ->Evaluated
lowest t = foldl1 (\(p1, f1, s1) (p2, f2, s2) -> if f1 < f2 then (p1, f1, s1) else (p2, f2, s2)) t

evaluate' :: GameHistory -> Evaluated
evaluate' gh = (gh, evaluateGH gh, determineStatus gh)

evaluate :: Position -> Float
evaluate p = whitePieces' p - blackPieces' p + pawnAdvancement p + development p + safeKing p

evaluateGH :: GameHistory -> Float
evaluateGH gh = isMate gh + evaluate (head gh)

isMate :: GameHistory -> Float
isMate gh
  | toPlay gh == White && isCheckMate gh = 99999.0
  | toPlay gh == Black && isCheckMate gh = (-99999.0)
  | otherwise = 0.0


safeKing :: Position -> Float
safeKing p
  | (pieceAt p ('g', 1) == Just (King White)) && (pieceAt p ('f', 1) == Just (Rook White)) = 0.1
  | (pieceAt p ('c', 1) == Just (King White)) && (pieceAt p ('d', 1) == Just (Rook White)) = 0.1
  | (pieceAt p ('g', 8) == Just (King Black)) && (pieceAt p ('f', 8) == Just (Rook Black)) = (-0.1)
  | (pieceAt p ('c', 8) == Just (King Black)) && (pieceAt p ('d', 8) == Just (Rook Black)) = (-0.1)
  | otherwise = 0.0

development :: Position -> Float
development p = sum $ fmap scoreOfficerDevelopment p

scoreOfficerDevelopment :: (Square, Maybe Piece) -> Float
scoreOfficerDevelopment ((col, row), Just (Knight White)) = if (row == 1) then 0.0 else 0.07
scoreOfficerDevelopment ((col, row), Just (Knight Black)) = if (row == 8) then 0.0 else (-0.07)
scoreOfficerDevelopment ((col, row), Just (Bishop White)) = if (row == 1) then 0.0 else 0.06
scoreOfficerDevelopment ((col, row), Just (Bishop Black)) = if (row == 8) then 0.0 else (-0.06)
scoreOfficerDevelopment ((col, row), Just (Rook White)) = if (row == 1) then 0.0 else 0.05
scoreOfficerDevelopment ((col, row), Just (Rook Black)) = if (row == 8) then 0.0 else (-0.05)
scoreOfficerDevelopment x = 0.0

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

first :: GameHistory -> GameHistory
first gh = head (Chess.positionTree gh) : gh