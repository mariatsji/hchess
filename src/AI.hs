module AI (
    bestDeepEval,
    bestMove,
) where

import Chess (
    Status (..),
    determineStatus,
    positionTree,
    (<-&->),
 )
import Evaluation (deepEval, alphaBeta)
import Position (Color (White), Position, next, toPlay)

import Data.Foldable (maximumBy, minimumBy)
import Relude

bestMove :: Position -> Int -> (Maybe Position, Maybe Float, Status)
bestMove = bestDeepEval

bestDeepEval :: Position -> Int -> (Maybe Position, Maybe Float, Status)
bestDeepEval pos' depth =
    let perspective = toPlay pos'
        candidates = positionTree pos' -- threefold?
        withScores = candidates <-&-> \p -> (p, alphaBeta depth (-10000) 10000 (next perspective) p)
        (best, score) = if perspective == White then maximumBy (comparing snd) withScores else minimumBy (comparing snd) withScores
     in if null candidates
            then (Nothing, Nothing, determineStatus pos' candidates)
            else (Just best, Just score, determineStatus best (positionTree best))
