module AI
  ( bestDeepEval,
    bestMove,
  )
where

import Chess
  ( Status (..),
    determineStatus,
    positionTree,
    (<-&->),
  )
import Data.Foldable (maximumBy)
import Evaluation (alphaBeta)
import Position (Color (White), Position, next, toPlay)
import Relude

bestMove :: Position -> Int -> (Maybe Position, Maybe Float, Status)
bestMove = bestDeepEval

bestDeepEval :: Position -> Int -> (Maybe Position, Maybe Float, Status)
bestDeepEval pos' depth =
  let perspective = toPlay pos'
      candidates = positionTree pos'
      withScores = candidates <-&-> \p -> (p, -alphaBeta depth (-10000) 10000 (next perspective) p)
      (best, negamaxScore) = maximumBy (comparing snd) withScores
      -- Convert from negamax (relative to mover) to absolute (positive = white advantage)
      absScore = if perspective == White then negamaxScore else -negamaxScore
   in if null candidates
        then (Nothing, Nothing, determineStatus pos' candidates)
        else (Just best, Just absScore, determineStatus best (positionTree best))
