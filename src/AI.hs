module AI (bestSearchedGH, best, positionTreeSearch) where

import Data.List
import Data.Ord
import Chess
import Evaluation

-- focused :: GameHistory -> Int -> GameHistory
-- focused gh depth = focused' (determinedStatus gh, gh) depth

-- takes a status and gamehistory and a perspective (black or white) and a search depth. recurs.
focused' :: (Status, GameHistory) -> Int -> [(Status, GameHistory)]
focused' (status, gh) 0         = [(status, gh)]
focused' (Remis, gh) _          = [(Remis, gh)]
focused' (WhiteIsMate, gh) _    = [(WhiteIsMate, gh)]
focused' (BlackIsMate, gh) _    = [(BlackIsMate, gh)]
focused' (WhiteToPlay, gh) d    = fmap toGH (highest' 10 (fmap evaluate' (positionTree' gh))) >>= (\(s', gh') -> focused' (determineStatus gh', gh') (d - 1))
focused' (BlackToPlay, gh) d    = fmap toGH (lowest'  10 (fmap evaluate' (positionTree' gh))) >>= (\(s', gh') -> focused' (determineStatus gh', gh') (d - 1))

highest' :: Int -> [Evaluated] -> [Evaluated]
highest' cutoff e = take cutoff $ sortBy comp e
  where comp e1 e2 = comparing (\(_,x,_) -> x) e1 e2

lowest' :: Int -> [Evaluated] -> [Evaluated]
lowest' cutoff e = take cutoff $ sortBy comp e
  where comp e1 e2 = comparing (\(_,x,_) -> negate x) e1 e2

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

