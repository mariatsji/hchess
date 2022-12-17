module AI (
    bestDeepEval,
) where

import Chess (
    Status (..),
    determineStatus,
    positionTree,
    (<-&->),
 )
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Evaluation (deepEval)
import Position (Color (White), Position (toPlay), next)

-- black to move
bestDeepEval :: Position -> Int -> (Maybe Position, Maybe Float, Status)
bestDeepEval pos' depth =
    let perspective = toPlay pos'
        candidates = positionTree pos' -- white to move
        withScores = candidates <-&-> \p -> (p, deepEval depth (next perspective) p)
        (best, score) = if perspective == White then maximumBy (comparing snd) withScores else minimumBy (comparing snd) withScores
     in if null candidates
            then (Nothing, Nothing, determineStatus pos')
            else (Just best, Just score, determineStatus best)
