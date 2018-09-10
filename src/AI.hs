{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE OverloadedLists #-}

module AI
  ( focused'
  , focused
  , focusedBest
  , edgeGreed
  , highestSoFar
  ) where

import           Chess
import           Data.List
import           Data.Ord
import           Evaluation

edgeGreed :: GameHistory -> Int -> Either (GameHistory, Status) GameHistory
edgeGreed gh depth =
  let -- startEvaluation :: Evaluated
      -- startEvaluation = evaluate' $! head $ expandHorizon 1 gh 
      bestWithinHorizon :: Evaluated
      bestWithinHorizon = foldr1 (swapForBetter (toPlay gh)) $! evaluate' <$> expandHorizon depth gh
      bestAsGH :: GameHistory
      bestAsGH = getGH bestWithinHorizon
      ghOneStep' :: GameHistory
      ghOneStep' = ghOneStep gh bestAsGH
  in if length bestAsGH > length gh + 1
    then Right bestAsGH
    else Left (bestAsGH, getStatus bestWithinHorizon)


-- compare current potential gh from horizon with a best so far (used in a fold over complete horizon)
swapForBetter :: Color -> Evaluated -> Evaluated -> Evaluated
swapForBetter White ePot@(Evaluated ghPot scorePot statusPot) bestSoFar@(Evaluated ghBSF scoreBSF statusBSC) = if scorePot > scoreBSF then ePot else bestSoFar
swapForBetter Black ePot@(Evaluated ghPot scorePot statusPot) bestSoFar@(Evaluated ghBSF scoreBSF statusBSC) = if scorePot < scoreBSF then ePot else bestSoFar

expandHorizon :: Int -> GameHistory -> [GameHistory]
expandHorizon 0 gh = []
expandHorizon 1 gh = positionTree' gh
expandHorizon n gh = -- positionTree' gh >>= expandHorizon (n - 1)
  -- best so far : foldl (\a c -> expandHorizon (n - 1) c) [] (positionTree' gh)
  foldl (\a c -> expandHorizon (n - 1) c) [] (positionTree' gh)

-- best give you Either Status or a gh ++ Position (gh with next position in it)
focusedBest :: GameHistory -> Int -> Either (GameHistory, Status) GameHistory
focusedBest gh depth =
  let ghPotential = ghFromE $ focused gh depth
      ghFromE (Evaluated a _ _) = a
      ghOneStep' = ghOneStep gh ghPotential
      status = determineStatus ghPotential
   in if length ghPotential > length gh + 1
        then Right ghOneStep'
        else Left (ghOneStep', status)

-- only evaluate the n most promising replies (which is to say every reply..)
searchWidth :: Int
searchWidth = 1000

--give you a full gh (i.e. not only next position)
focused :: GameHistory -> Int -> Evaluated -- this is maybe grap
focused gh depth
  | toPlay gh == White =
    head $ highest' searchWidth (focused' (evaluate' gh) (depth, 3))
  | otherwise = head $ lowest' searchWidth (focused' (evaluate' gh) (depth, 3))

-- takes a status and gamehistory and a perspective (black or white) and a search (depth, width). recurs. gives full gh (i.e. not only next position)
focused' :: Evaluated -> (Int, Int) -> [Evaluated]
focused' !e (0, _) = [e]
focused' (Evaluated !gh _ WhiteToPlay) (d, w) =
  highest' w (evaluate'' (positionTree gh) gh) >>= flip focused' (d - 1, w)
focused' (Evaluated !gh _ BlackToPlay) (d, w) =
  lowest' w (evaluate'' (positionTree gh) gh) >>= flip focused' (d - 1, w)
focused' !e _ = [e]

highest' :: Int -> [Evaluated] -> [Evaluated]
highest' cutoff = foldr (highestSoFar cutoff) []

highestSoFar :: Int -> Evaluated -> [Evaluated] -> [Evaluated]
highestSoFar i ev soFar
  | length soFar < i = ev : soFar
  | otherwise = replaceLowest ev soFar

replaceLowest :: Evaluated -> [Evaluated] -> [Evaluated]
replaceLowest e [] = [e]
replaceLowest e (e':ex) =
  if getScore e < getScore e'
    then e' : ex
    else replaceLowest e ex

lowest' :: Int -> [Evaluated] -> [Evaluated]
lowest' cutoff = foldr (lowestSoFar cutoff) []

lowestSoFar :: Int -> Evaluated -> [Evaluated] -> [Evaluated]
lowestSoFar i ev soFar
  | length soFar < i = ev : soFar
  | otherwise = replaceHighest ev soFar

replaceHighest :: Evaluated -> [Evaluated] -> [Evaluated]
replaceHighest e [] = [e]
replaceHighest e (e':ex) =
  if getScore e > getScore e'
    then e' : ex
    else replaceHighest e ex

getScore :: Evaluated -> Float
getScore (Evaluated _ x _) = x

getGH :: Evaluated -> GameHistory
getGH (Evaluated x _ _ ) = x

getStatus :: Evaluated -> Status
getStatus (Evaluated _ _ x) = x

ghOneStep :: GameHistory -> GameHistory -> GameHistory
ghOneStep [] _ = []
ghOneStep _ [] = []
ghOneStep (x:xsshort) (y:yslong) =
  if yslong == (x : xsshort)
    then y : yslong
    else ghOneStep (x : xsshort) yslong
