module AI
  ( focused'
  , focused
  , focusedBest
  ) where

import           Chess
import           Control.Parallel (par, pseq)
import           Data.List
import           Data.Ord
import           Evaluation

-- best give you Either Status or a gh ++ Position (gh with next position in it)
focusedBest :: GameHistory -> Int -> Either (GameHistory, Status) GameHistory
focusedBest gh depth =
  let ghPotential = ghFromE $ focused gh depth
      ghFromE (a, _, _) = a
   in if length ghPotential > length gh + 1
        then Right (ghOneStep gh ghPotential)
        else Left (ghOneStep gh ghPotential, determineStatus ghPotential)

-- only evaluate the n most promising replies (which is to say every reply..)
searchWidth :: Int
searchWidth = 1000

--give you a full gh (i.e. not only next position)
focused :: GameHistory -> Int -> Evaluated -- this is maybe grap
focused gh depth
  | toPlay gh == White = head $ highest' searchWidth (focused' (evaluate' gh) (depth, 3))
  | otherwise = head $ lowest' searchWidth (focused' (evaluate' gh) (depth, 3))

-- par greaterForced $ pseq lesserForced  $ lesser ++ x:greater
-- takes a status and gamehistory and a perspective (black or white) and a search (depth, width). recurs. gives full gh (i.e. not only next position)
focused' :: Evaluated -> (Int, Int) -> [Evaluated]
focused' e (0, _) = [e]
focused' (gh, _, WhiteToPlay) (d, w) =
  highest' w firstHalfLazy ++ secondHalfLazy >>= flip focused' (d - 1, w)
  where
    allEvaluated = evaluate'' (positionTree gh) gh
    firstHalfLazy = take (length allEvaluated `div` 2) allEvaluated
    secondHalfLazy = drop (length allEvaluated `div` 2) allEvaluated
focused' (gh, _, BlackToPlay) (d, w) =
  lowest' w (evaluate'' (positionTree gh) gh) >>= flip focused' (d - 1, w)
focused' e _ = [e]

highest' :: Int -> [Evaluated] -> [Evaluated]
highest' cutoff e = take cutoff $ sortBy comp e
  where
    comp = comparing (\(_, x, _) -> negate x)

lowest' :: Int -> [Evaluated] -> [Evaluated]
lowest' cutoff e = take cutoff $ sortBy comp e
  where
    comp = comparing (\(_, x, _) -> x)

ghOneStep :: GameHistory -> GameHistory -> GameHistory
ghOneStep [] _ = []
ghOneStep _ [] = []
ghOneStep (x:xsshort) (y:yslong) =
  if yslong == (x : xsshort)
    then y : yslong
    else ghOneStep (x : xsshort) yslong
