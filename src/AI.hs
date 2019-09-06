{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module AI
  ( focused',
    focused,
    focusedBest,
    edgeGreed,
    streamBest,
    highestSoFar,
    expandHorizon,
    swapForBetter,
    oneStep
    )
where

import Chess
import Conduit
import Control.Monad
import Data.List
import Data.Ord
import Evaluation
import Prelude hiding (foldr)

streamBest :: Position -> Int -> Either (Position, Status) Position
streamBest pos depth =
  let bestWithinHorizon =
        runConduitPure
          $ yieldMany (expandHorizon depth pos)
          .| mapC evaluate'
          .| foldlC (swapForBetter (toPlay pos)) (evaluate' pos)
      best = getPos bestWithinHorizon
      oneStep' = oneStep pos best
   in if length (gamehistory best) > length (gamehistory pos) + 1
        then Right oneStep'
        else Left (oneStep', getStatus bestWithinHorizon)

edgeGreed :: Position -> Int -> Either (Position, Status) Position
edgeGreed !pos depth =
  -- startEvaluation :: Evaluated
  -- startEvaluation = evaluate' $! head $ expandHorizon 1 gh
  let bestWithinHorizon :: Evaluated
      -- print =  unsafePerformIO $ prettyEs $ map evaluate' $ expandHorizon depth gh
      bestWithinHorizon =
        foldr1 (swapForBetter (toPlay pos))
          $! map evaluate'
          $ expandHorizon depth pos
      best = getPos bestWithinHorizon
      oneStep' = oneStep pos best
   in if length (gamehistory best) > length (gamehistory pos) + 1
        then Right oneStep'
        else Left (oneStep', getStatus bestWithinHorizon)

-- compare current potential gh from horizon with a best so far (used in a fold over complete horizon)
swapForBetter :: Color -> Evaluated -> Evaluated -> Evaluated
swapForBetter White ePot@(Evaluated _ scorePot _) bestSoFar@(Evaluated _ scoreBSF _) =
  if scorePot > scoreBSF
    then ePot
    else bestSoFar
swapForBetter Black ePot@(Evaluated _ scorePot _) bestSoFar@(Evaluated _ scoreBSF _) =
  if scorePot < scoreBSF
    then ePot
    else bestSoFar

expandHorizon :: Int -> Position -> [Position]
expandHorizon 0 _ = undefined
expandHorizon 1 !pos = positionTree pos
expandHorizon n !pos = expandHorizon 1 pos >>= expandHorizon (n - 1)

-- best give you Either Status or a gh ++ Position (gh with next position in it)
focusedBest :: Position -> Int -> Either (Position, Status) Position
focusedBest pos depth =
  let posPotential = posFromE $ focused pos depth
      posFromE (Evaluated a _ _) = a
      posOneStep' = oneStep pos posPotential
      status = determineStatus posPotential
   in if length (gamehistory posPotential) > length (gamehistory pos) + 1
        then Right posOneStep'
        else Left (posOneStep', status)

-- only evaluate the n most promising replies (which is to say every reply..)
searchWidth :: Int
searchWidth = 1000

--give you a full gh (i.e. not only next position)
focused :: Position -> Int -> Evaluated -- this is maybe grap
focused pos depth
  | toPlay pos == White =
    head $ highest' searchWidth (focused' (evaluate' pos) (depth, 3))
  | otherwise = head $ lowest' searchWidth (focused' (evaluate' pos) (depth, 3))

-- takes a status and gamehistory and a perspective (black or white) and a search (depth, width). recurs. gives full gh (i.e. not only next position)
focused' :: Evaluated -> (Int, Int) -> [Evaluated]
focused' !e (0, _) = [e]
focused' (Evaluated !pos _ WhiteToPlay) (d, w) =
  highest' w (evaluate'' (positionTree pos)) >>= flip focused' (d - 1, w)
focused' (Evaluated !pos _ BlackToPlay) (d, w) =
  lowest' w (evaluate'' (positionTree pos)) >>= flip focused' (d - 1, w)
focused' !e _ = [e]

highest' :: Int -> [Evaluated] -> [Evaluated]
highest' cutoff = foldr (highestSoFar cutoff) []

highestSoFar :: Int -> Evaluated -> [Evaluated] -> [Evaluated]
highestSoFar i ev soFar
  | length soFar < i = ev : soFar
  | otherwise = replaceLowest ev soFar

replaceLowest :: Evaluated -> [Evaluated] -> [Evaluated]
replaceLowest e [] = [e]
replaceLowest e (e' : ex) =
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
replaceHighest e (e' : ex) =
  if getScore e > getScore e'
    then e' : ex
    else replaceHighest e ex

getScore :: Evaluated -> Float
getScore (Evaluated _ x _) = x

getPos :: Evaluated -> Position
getPos (Evaluated x _ _) = x

getStatus :: Evaluated -> Status
getStatus (Evaluated _ _ x) = x

oneStep :: Position -> Position -> Position
oneStep (Position ma gha) long@(Position mb ghb) =
  let nextPos = (mb : ghb) !! (length (mb : ghb) - length (ma : gha) - 1)
   in long {m = nextPos, gamehistory = ma : gha}
