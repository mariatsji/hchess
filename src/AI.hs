{-# LANGUAGE OverloadedLists #-}

module AI (
    edgeGreed,
) where

import Cache
import Chess (
    Status (..),
    determineStatus,
    positionTree,
    (<-$->),
 )
import Control.Monad (Monad ((>>=)))
import Control.Parallel (par, pseq)
import Control.Parallel.Strategies (Eval, NFData, rdeepseq, rpar, rseq, runEval)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (foldl', maximumBy, minimumBy)
import Data.Functor ((<&>))
import Data.List (drop, elem, find, length, sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Ord ((<), (>)), comparing)
import qualified Debug.Trace as Debug
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
import Printer (prettyE)

-- black to move
edgeGreed :: Position -> Int -> Either (Position, Status) Position
edgeGreed pos' depth =
    let status = determineStatus pos'
     in if terminal status
            then Left (pos', status)
            else
                let perspective = toPlay pos'
                    candidates = positionTree pos' -- white to move
                    withScores = candidates <&> \p -> (p, deepEval depth (next perspective) p)
                    (best, score) = if perspective == White then maximumBy (comparing snd) withScores else minimumBy (comparing snd) withScores
                 in Debug.traceShow (toPlay pos') $ Right best
