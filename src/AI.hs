{-# LANGUAGE OverloadedLists #-}

module AI (
    edgeGreed,
    oneStep,
    best,
    dig,
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
import Data.Foldable (foldl')
import Data.List (drop, elem, find, length, sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Ord ((<), (>)))
import qualified Debug.Trace as Debug
import Evaluation (Evaluated (..), evaluate', getPosition)
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

-- restore edgeGreed until oneStep is in place
edgeGreed :: Position -> Int -> Either (Position, Status) Position
edgeGreed pos depth = edgeGreedCompat pos depth 200

-- silly wrapper, this is not edgeGreed but dig..
edgeGreedCompat :: Position -> Int -> Int -> Either (Position, Status) Position
edgeGreedCompat pos' depth broadness =
    let status = determineStatus pos'
     in if terminal status
            then Left (pos', status)
            else
                let perspective = toPlay pos'
                    candidates = positionTree pos'
                    ranked = dig depth broadness perspective <$> candidates
                    chosen = singleBest perspective ranked
                 in maybe
                        (error "")
                        (Right . oneStep pos' . pos)
                        chosen

terminal :: Status -> Bool
terminal = \case
    WhiteIsMate -> True
    BlackIsMate -> True
    Remis -> True
    _ -> False

-- get a deep score for one position
dig :: Int -> Int -> Color -> Position -> Evaluated
dig depth broadness perspective pos' =
    let status = determineStatus pos'
        candidates = positionTree pos'
     in if terminal status
            then evaluate' pos'
            else
                let evaluated =
                        if depth == 1
                            then evaluate' <-$-> candidates
                            else dig (depth - 1) broadness (next perspective) <-$-> candidates -- these candidates are all as good as further dig sais they are
                 in fromMaybe
                        (error $ "no candidates at depth " <> show depth)
                        (singleBest perspective evaluated)

singleBest :: Color -> [Evaluated] -> Maybe Evaluated
singleBest color cands =
    listToMaybe $ best 1 color cands

best :: Int -> Color -> [Evaluated] -> [Evaluated]
best n perspective evals =
    take n $ sortBy (sorter perspective) evals

sorter :: Color -> Evaluated -> Evaluated -> Ordering
sorter perspective (Evaluated posA scoreA statusA) (Evaluated posB scoreB statusB) =
    if perspective == White
        then case (statusA, statusB) of
            (BlackIsMate, BlackIsMate) -> EQ
            (WhiteIsMate, WhiteIsMate) -> EQ
            (BlackIsMate, _) -> GT
            (_, BlackIsMate) -> LT
            (WhiteIsMate, _) -> LT
            (_, WhiteIsMate) -> GT
            (_, _) -> compare scoreB scoreA -- todo why does White want to flip here? I dont get it
        else case (statusA, statusB) of
            (WhiteIsMate, WhiteIsMate) -> EQ
            (BlackIsMate, BlackIsMate) -> EQ
            (WhiteIsMate, _) -> GT
            (_, WhiteIsMate) -> LT
            (BlackIsMate, _) -> LT
            (_, BlackIsMate) -> GT
            (_, _) -> compare scoreA scoreB -- black wants as negative as possible

-- todo this throws away sooooo much info!
oneStep :: Position -> Position -> Position
oneStep p (Position snpAdvanced ghAdvanced _ _ _) =
    let snp = m p
        gh = gamehistory p
        idx = (length ghAdvanced - length gh) - 1
     in case safeLookup idx (snpAdvanced : ghAdvanced) of
            Nothing -> error $ "Could not lookup oneStep, from gh length " <> show (length gh) <> " to length advanced " <> show (length ghAdvanced)
            Just nextSnp -> mkPositionExpensive p nextSnp

safeLookup :: Int -> [a] -> Maybe a
safeLookup _ [] = Nothing
safeLookup 0 (x : _) = Just x
safeLookup i (_ : xs) = safeLookup (i - 1) xs
