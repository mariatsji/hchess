{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists #-}

module AI
  ( edgeGreed,
    expandHorizon,
    swapForBetter,
    oneStep,
  )
where

import Chess
import Control.Monad
import Control.Parallel
import Data.List
import Data.Maybe (listToMaybe)
import Data.Ord
import Evaluation
import Position
import Prelude hiding (foldr)

edgeGreed :: Position -> Int -> Either (Position, Status) Position
edgeGreed !pos depth =
  let color = toPlay pos
      best =
        foldr
          ( \potential bestsofar ->
              potential `seq` bestsofar `seq`
              swapForBetter color (evaluate' potential) bestsofar
          )
          (evaluate' pos)
          (expandHorizon depth pos)
   in maybe (Left (pos, determineStatus pos)) Right (oneStep pos (getPosition best))

-- compare current potential gh from horizon with a best so far (used in a fold over complete horizon)
swapForBetter :: Color -> Evaluated -> Evaluated -> Evaluated
swapForBetter White ePot@(Evaluated _ scorePot _) bestSoFar@(Evaluated _ scoreBSF statusBSF)
  | statusBSF == BlackIsMate = bestSoFar
  | scorePot > scoreBSF = ePot
  | otherwise = bestSoFar
swapForBetter Black ePot@(Evaluated _ scorePot _) bestSoFar@(Evaluated _ scoreBSF statusBSF)
  | statusBSF == WhiteIsMate = bestSoFar
  | scorePot < scoreBSF = ePot
  | otherwise = bestSoFar

expandHorizon :: Int -> Position -> [Position]
expandHorizon 0 _ = error "cannot expand horizon 0 steps"
expandHorizon 1 pos = positionTree pos
expandHorizon n pos = let expanded = expandHorizon 1 pos
  in case expanded of
    [] -> []
    [x,y] -> let first = expandHorizon (n - 1) x
                 second = expandHorizon (n - 1) y
              in first `pseq` second
    (x:xs) -> let first = expandHorizon (n - 1) x
              in first `par` first <> (xs >>= expandHorizon (n - 1))

oneStep :: Position -> Position -> Maybe Position
oneStep pos@(Position snpa gha _ _ _ _) (Position snpb ghb _ _ _ _) =
  let diff = length (snpb : ghb) - length (snpa : gha)
      onemorelist = drop (diff - 1) (snpb : ghb)
   in if diff > 0 then mkPositionExpensive pos <$> listToMaybe onemorelist else Nothing
