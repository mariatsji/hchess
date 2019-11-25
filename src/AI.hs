{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module AI
  ( focused',
    focused,
    edgeGreed,
    highestSoFar,
    expandHorizon,
    swapForBetter,
    oneStep
    )
where

import Bunch
import Chess
import Control.Monad
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List
import Data.Ord
import Evaluation
import Position
import Prelude hiding (foldr)

edgeGreed :: Position -> Int -> Either (Position, Status) Position
edgeGreed !pos depth =
  let color = toPlay pos
      best =
       foldr
        (
          \potential bestsofar -> 
          swapForBetter color (evaluate' potential) bestsofar
        )
        (evaluate' pos)
        (expandHorizon depth pos)
  in maybe (Left (pos, determineStatus pos)) Right (oneStep pos (getPosition best))
  

-- compare current potential gh from horizon with a best so far (used in a fold over complete horizon)
swapForBetter :: Color -> Evaluated -> Evaluated -> Evaluated
swapForBetter White ePot@(Evaluated _ scorePot statusPot) bestSoFar@(Evaluated _ scoreBSF statusBSF)
  | statusBSF == BlackIsMate = bestSoFar
  | scorePot > scoreBSF = ePot
  | otherwise = bestSoFar
swapForBetter Black ePot@(Evaluated _ scorePot _) bestSoFar@(Evaluated _ scoreBSF statusBSF)
  | statusBSF == WhiteIsMate = bestSoFar
  | scorePot < scoreBSF = ePot
  | otherwise = bestSoFar

expandHorizon :: Int -> Position -> Bunch Position
expandHorizon 0 _ = error "cannot expand horizon 0 steps"
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

oneStep :: Position -> Position -> Maybe Position
oneStep pos@(Position snpa gha _ _ _ _) (Position snpb ghb _ _ _ _) =
  let diff = length (snpb : ghb) - length (snpa : gha)
      onemorelist = drop (diff - 1) (snpb : ghb)
  in if diff > 0 then mkPositionExpensive pos <$> listToMaybe onemorelist else Nothing
  