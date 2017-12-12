module AI (focused', focused, focusedBest) where

import Data.List
import Data.Ord
import Chess
import Evaluation

-- best give you Either Status or a gh + Position (gh with next position in it)
focusedBest :: GameHistory -> Int -> Either (GameHistory, Status) GameHistory
focusedBest gh depth =
  let ghPotential = ghFromE $ focused gh depth
      ghFromE = (\(a,_,_) -> a)
  in
   if (length ghPotential > length gh + 1) then Right (ghOneStep gh ghPotential) else Left (ghOneStep gh ghPotential, determineStatus ghPotential)

--give you a full gh (i.e. not only next position)
focused :: GameHistory -> Int -> Evaluated -- this is maybe grap
focused gh depth
  | (toPlay gh) == White = head $ highest' 1 (focused' (evaluate' gh) depth)
  | otherwise = head $ lowest' 1 (focused' (evaluate' gh) depth)

-- takes a status and gamehistory and a perspective (black or white) and a search depth. recurs. gives full gh (i.e. not only next position)
focused' :: Evaluated -> Int -> [Evaluated]
focused' e 0                      = [e]
focused' (gh, _, WhiteToPlay) d   = (highest' 5 (evaluate'' (positionTree gh) gh)) >>= (flip focused' (d - 1))
focused' (gh, _, BlackToPlay) d   = (lowest'  5 (evaluate'' (positionTree gh) gh)) >>= (flip focused' (d - 1))
focused' e _                      = [e]

highest' :: Int -> [Evaluated] -> [Evaluated]
highest' cutoff e = take cutoff $ sortBy comp e
  where comp e1 e2 = comparing (\(_,x,_) -> negate x) e1 e2

lowest' :: Int -> [Evaluated] -> [Evaluated]
lowest' cutoff e = take cutoff $ sortBy comp e
  where comp e1 e2 = comparing (\(_,x,_) -> x) e1 e2

ghOneStep :: GameHistory -> GameHistory -> GameHistory
ghOneStep [] _ = []
ghOneStep _ [] = []
ghOneStep (x:xsshort) (y:yslong) = if yslong == (x:xsshort) then y:yslong else ghOneStep (x:xsshort) yslong

