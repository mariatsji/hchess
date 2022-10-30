{-# LANGUAGE OverloadedLists #-}

module AI (
    edgeGreed,
    expandHorizon,
    swapForBetter,
    oneStep,
) where

import Chess (
    Status (BlackIsMate, WhiteIsMate),
    determineStatus,
    positionTree,
 )
import Control.Monad (Monad ((>>=)))
import Control.Parallel (par, pseq)
import Data.List (drop, length)
import Data.Maybe (listToMaybe)
import Data.Ord (Ord ((<), (>)))
import Evaluation (Evaluated (..), evaluate', getPosition)
import Position (
    Color (..),
    Position (Position, toPlay),
    mkPositionExpensive,
 )
import Data.Foldable (foldl')

edgeGreed :: Position -> Int -> Either (Position, Status) Position
edgeGreed pos depth =
    let status = determineStatus pos
        color = toPlay pos
        best =
            foldl'
                ( \bestsofar potential ->
                    potential `pseq` bestsofar `par`
                    swapForBetter color (evaluate' potential status) bestsofar
                )
                (evaluate' pos status)
                (expandHorizon depth pos)
     in maybe (Left (pos, status)) Right (oneStep pos (getPosition best))

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
expandHorizon n pos =
    let expanded = expandHorizon 1 pos
     in case expanded of
            [] -> []
            [x, y] ->
                let first = expandHorizon (n - 1) x
                    second = expandHorizon (n - 1) y
                 in first `pseq` second
            (x : xs) ->
                let first = expandHorizon (n - 1) x
                 in first `par` first <> (xs >>= expandHorizon (n - 1))

oneStep :: Position -> Position -> Maybe Position
oneStep pos@(Position snpa gha _ _ _ _ _) (Position snpb ghb _ _ _ _ _) =
    let diff = length (snpb : ghb) - length (snpa : gha)
        onemorelist = drop (diff - 1) (snpb : ghb)
     in if diff > 0 then mkPositionExpensive pos <$> listToMaybe onemorelist else Nothing
