module AI (bestSearchedGH, best, positionTreeSearch, focused', focused, focusedBest) where

import Data.List
import Data.Ord
import Chess
import Evaluation

-- best give you Either Status or a gh + Position (gh with next position in it)
focusedBest :: GameHistory -> Int -> Either Status GameHistory
focusedBest gh depth =
  let ghPotential = ghFromE $ focused gh depth
      ghFromE = (\(a,_,_) -> a)
  in
   if (length ghPotential > length gh + 1) then Right (ghOneStep gh ghPotential) else Left (determineStatus ghPotential)

--give you a full gh (i.e. not only next position)
focused :: GameHistory -> Int -> Evaluated -- this is maybe grap
focused gh depth
  | (toPlay gh) == White = head $ highest' 1 (focused' (evaluate' gh) depth)
  | otherwise = head $ lowest' 1 (focused' (evaluate' gh) depth)

-- takes a status and gamehistory and a perspective (black or white) and a search depth. recurs. gives full gh (i.e. not only next position)
focused' :: Evaluated -> Int -> [Evaluated]
focused' e 0                      = [e]
focused' (gh, f, WhiteToPlay) d   = (highest' 5 (fmap evaluate' (positionTree' gh))) >>= (flip focused' (d - 1))
focused' (gh, f, BlackToPlay) d   = (lowest'  5 (fmap evaluate' (positionTree' gh))) >>= (flip focused' (d - 1))
focused' e _                      = [e]

highest' :: Int -> [Evaluated] -> [Evaluated]
highest' cutoff e = take cutoff $ sortBy comp e
  where comp e1 e2 = comparing (\(_,x,_) -> negate x) e1 e2

lowest' :: Int -> [Evaluated] -> [Evaluated]
lowest' cutoff e = take cutoff $ sortBy comp e
  where comp e1 e2 = comparing (\(_,x,_) -> x) e1 e2

-- best give you Either Status or a gh + Position (gh with next position in it)
best :: GameHistory -> Int -> Either Status GameHistory
best gh depth =
  let ghPotential = ghFromE $ bestSearchedGH gh depth
      ghFromE = (\(a,_,_) -> a)
  in
   if (length ghPotential > length gh + 1) then Right (ghOneStep gh ghPotential) else Left (determineStatus ghPotential)

highest :: [Evaluated] -> Evaluated
highest t = foldl1 (\(p1, f1, s1) (p2, f2, s2) -> if f1 > f2 then (p1, f1, s1) else (p2, f2, s2)) t

lowest :: [Evaluated] ->Evaluated
lowest t = foldl1 (\(p1, f1, s1) (p2, f2, s2) -> if f1 < f2 then (p1, f1, s1) else (p2, f2, s2)) t

ghOneStep :: GameHistory -> GameHistory -> GameHistory
ghOneStep [] _ = []
ghOneStep _ [] = []
ghOneStep (x:xsshort) (y:yslong) = if yslong == (x:xsshort) then y:yslong else ghOneStep (x:xsshort) yslong

-- gives you the full potential gamehistory for the current gamehistory, as given by search depth
bestSearchedGH :: GameHistory -> Int -> Evaluated
bestSearchedGH gh depth
  | toPlay gh == White = highest $ fmap evaluateS $ positionTreeSearch gh depth
  | otherwise = lowest $ fmap evaluateS $ positionTreeSearch gh depth

positionTreeSearch :: GameHistory -> Int -> [(GameHistory, Status)]
positionTreeSearch gh 0 = [(gh, determineStatus gh)]
positionTreeSearch gh depth = foldl (\a _ -> a >>= positionTreeS) (positionTreeS (gh, determineStatus gh)) (take (depth - 1) $ [1 ..] :: [Int])

