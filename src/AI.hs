module AI (
    edgeGreed,
) where

import Cache
import Chess (
    Status (..),
    determineStatus,
    positionTree,
    (<-$->),
    (<-&->),
 )
import Control.Monad (Monad ((>>=)))
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (Eval, NFData, rdeepseq, rpar, rseq, runEval)
import Data.Foldable (foldl', maximumBy, minimumBy)
import Data.Functor ((<&>))
import Data.List (drop, elem, find, length, sortBy)
import Data.Ord (Ord ((<), (>)), comparing)
import Evaluation (Evaluated (..), deepEval, evaluate, evaluate', getPosition, terminal)
import Move (playMoves)
import Position (
    Color (..),
    Move,
    Position (Position, gamehistory, m, toPlay),
    findMove,
    mkPositionExpensive,
    next,
 )

-- black to move
edgeGreed :: Position -> Int -> (Maybe Position, Status)
edgeGreed pos' depth =
    let perspective = toPlay pos'
        candidates = positionTree pos' -- white to move
        withScores = candidates <-&-> \p -> (p, deepEval depth (next perspective) p)
        (best, score) = if perspective == White then maximumBy (comparing snd) withScores else minimumBy (comparing snd) withScores
     in if null candidates
            then (Nothing, determineStatus pos')
            else (Just best, determineStatus best)
