{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}

module AI
  ( focused'
  , focused
  , focusedBest
  , highestSoFar
  ) where

import           Chess
import           Data.List
import           Data.Ord
import           Evaluation

-- best give you Either Status or a gh ++ Position (gh with next position in it)
focusedBest :: GameHistory -> Int -> Either (GameHistory, Status) GameHistory
focusedBest gh depth =
  let ghPotential = ghFromE $ focused gh depth
      ghFromE (Evaluated a _ _ ) = a
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
  | toPlay gh == White = head $ highest' searchWidth (focused' (evaluate' gh) (depth, 3))
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
replaceLowest e (e':ex) = if getScore e < getScore e' then e' : ex else replaceLowest e ex

lowest' :: Int -> [Evaluated] -> [Evaluated]
lowest' cutoff = foldr (lowestSoFar cutoff) []

lowestSoFar :: Int -> Evaluated -> [Evaluated] -> [Evaluated]
lowestSoFar i ev soFar
  | length soFar < i = ev : soFar
  | otherwise = replaceHighest ev soFar

replaceHighest :: Evaluated -> [Evaluated] -> [Evaluated]
replaceHighest e [] = [e]
replaceHighest e (e':ex) = if getScore e > getScore e' then e' : ex else replaceHighest e ex

getScore :: Evaluated -> Float
getScore (Evaluated _ x _) = x

ghOneStep :: GameHistory -> GameHistory -> GameHistory
ghOneStep [] _ = []
ghOneStep _ [] = []
ghOneStep (x:xsshort) (y:yslong) =
  if yslong == (x : xsshort)
    then y : yslong
    else ghOneStep (x : xsshort) yslong
