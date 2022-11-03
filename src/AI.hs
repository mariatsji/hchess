{-# LANGUAGE OverloadedLists #-}

module AI (
    edgeGreed,
    oneStep,
    best,
    dig,
) where

import Chess (
    Status (..),
    determineStatus,
    positionTree,
 )
import Control.Monad (Monad ((>>=)))
import Control.Parallel (par, pseq)
import Data.Either (partitionEithers)
import Data.Foldable (foldl')
import Data.List (drop, find, length, sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Ord ((<), (>)))
import Evaluation (Evaluated (..), evaluate', getPosition)
import Move (playMoves)
import Position (
    Color (..),
    Move,
    Position (Position, m, toPlay),
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
    let perspective = toPlay pos'
        evaluated = evaluate' pos'
        resEither = mkEither $ dig depth broadness (toPlay pos') evaluated
     in case resEither of
            Right ev -> case oneStep pos' (pos ev) of
                Just pozz -> Right pozz
                Nothing -> Left (pos', status ev)
            x -> fmap pos x
  where
    mkEither :: Evaluated -> Either (Position, Status) Evaluated
    mkEither ev@Evaluated {..} = case status of
        WhiteToPlay -> Right ev
        BlackToPlay -> Right ev
        terminalStatus -> Left (pos, terminalStatus)

-- oneStep-functionality is missing
dig :: Int -> Int -> Color -> Evaluated -> Evaluated
dig depth broadness perspective ev@Evaluated {..} =
    let candidates = positionTree pos
        evaluated = evaluate' <$> candidates
     in if depth == 0
            then fromMaybe ev $ singleBest perspective evaluated
            else case find (terminallyGood perspective) evaluated of
                Just x -> x
                Nothing ->
                    let furtherInspect = best broadness perspective evaluated
                     in fromMaybe ev $ singleBest perspective $ dig (depth - 1) broadness (next perspective) <$> furtherInspect

singleBest :: Color -> [Evaluated] -> Maybe Evaluated
singleBest color cands =
    listToMaybe $ best 1 color cands

terminallyGood :: Color -> Evaluated -> Bool
terminallyGood color Evaluated {..} = case color of
    White -> status == BlackIsMate
    Black -> status == WhiteIsMate

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

oneStep :: Position -> Position -> Maybe Position
oneStep pos@(Position snpa gha _ _ _) (Position snpb ghb _ _ _) =
    let diff = length (snpb : ghb) - length (snpa : gha)
        onemorelist = drop (diff - 1) (snpb : ghb)
     in if diff > 0 then mkPositionExpensive pos <$> listToMaybe onemorelist else Nothing
