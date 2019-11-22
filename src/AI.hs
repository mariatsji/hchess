{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module AI
  ( focused',
    focused,
    edgeGreed,
    streamBest,
    highestSoFar,
    expandHorizon,
    swapForBetter,
    oneStep
    )
where

import Bunch
import Chess
import Conduit
import Control.Monad
import Data.Maybe (listToMaybe)
import Data.List
import Data.Ord
import Evaluation
import Position
import Prelude hiding (foldr)

streamBest :: Position -> Int -> Either (Position, Status) Position
streamBest pos depth =
  let bestWithinHorizon =
        runConduitPure
          $ yieldMany (unBunch $ expandHorizon depth pos)
          .| mapC evaluate'
          .| foldlC (swapForBetter (toPlay pos)) (evaluate' pos)
      best = getPos bestWithinHorizon
      oneStep' = oneStep pos best
  in maybe (Left (pos, getStatus bestWithinHorizon)) Right oneStep'
   --in maybeToRight (pos, getStatus bestWithinHorizon) oneStep'

edgeGreed :: Position -> Int -> Either (Position, Status) Position
edgeGreed !pos depth =
  -- startEvaluation :: Evaluated
  -- startEvaluation = evaluate' $! head $ expandHorizon 1 gh
  let bestWithinHorizon :: Evaluated
      bestWithinHorizon = foldr1 (swapForBetter (toPlay pos)) (evaluate' <$> expandHorizon depth pos)
      best = getPos bestWithinHorizon
      oneStep' = oneStep pos best
   in maybe (Left (pos, getStatus bestWithinHorizon)) Right oneStep'

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

expandHorizon :: Int -> Position -> Bunch Position
expandHorizon 0 _ = undefined
expandHorizon 1 !pos = positionTree pos
expandHorizon n !pos = expandHorizon 1 pos >>= expandHorizon (n - 1)

-- only evaluate the n most promising replies (which is to say every reply..)
searchWidth :: Int
searchWidth = 1000

--give you a full gh (i.e. not only next position)
focused :: Position -> Int -> Evaluated -- this is maybe grap
focused pos depth
  | toPlay pos == White =
    unsafeHead $ highest' searchWidth (focused' (evaluate' pos) (depth, 3))
  | otherwise = unsafeHead $ lowest' searchWidth (focused' (evaluate' pos) (depth, 3))

-- takes a status and gamehistory and a perspective (black or white) and a search (depth, width). recurs. gives full gh (i.e. not only next position)
focused' :: Evaluated -> (Int, Int) -> Bunch Evaluated
focused' !e (0, _) = Bunch [e]
focused' (Evaluated !pos _ WhiteToPlay) (d, w) =
  highest' w (evaluate'' (positionTree pos)) >>= flip focused' (d - 1, w)
focused' (Evaluated !pos _ BlackToPlay) (d, w) =
  lowest' w (evaluate'' (positionTree pos)) >>= flip focused' (d - 1, w)
focused' !e _ = Bunch [e]

highest' :: Foldable f => Int -> f Evaluated -> Bunch Evaluated
highest' cutoff = foldr (highestSoFar cutoff) emptyBunch

highestSoFar :: Int -> Evaluated -> Bunch Evaluated -> Bunch Evaluated
highestSoFar i ev soFar
  | length soFar < i = Bunch (pure ev) <> soFar
  | otherwise = replaceLowest ev soFar

replaceLowest :: Foldable f => Evaluated -> f Evaluated -> Bunch Evaluated
replaceLowest e = foldl' (\acc c -> if getScore e < getScore c then singleton e <> acc else singleton c <> acc) emptyBunch

lowest' :: Foldable f => Int -> f Evaluated -> Bunch Evaluated
lowest' cutoff = foldr (lowestSoFar cutoff) emptyBunch

lowestSoFar :: Int -> Evaluated -> Bunch Evaluated -> Bunch Evaluated
lowestSoFar i ev soFar
  | length soFar < i = Bunch (pure ev) <> soFar
  | otherwise = replaceHighest ev soFar

replaceHighest :: Foldable f => Evaluated -> f Evaluated -> Bunch Evaluated
replaceHighest e = foldl' (\acc c -> if getScore e > getScore c then singleton e <> acc else singleton c <> acc) emptyBunch

getScore :: Evaluated -> Float
getScore (Evaluated _ x _) = x

getPos :: Evaluated -> Position
getPos (Evaluated x _ _) = x

getStatus :: Evaluated -> Status
getStatus (Evaluated _ _ x) = x

oneStep :: Position -> Position -> Maybe Position
oneStep pos@(Position snpa gha _ _ _ _) (Position snpb ghb _ _ _ _) =
  let diff = length (snpb : ghb) - length (snpa : gha)
      onemorelist = (drop (diff - 1) (snpb : ghb))
  in (\snp -> mkPositionExpensive pos snp ) <$> listToMaybe onemorelist
  