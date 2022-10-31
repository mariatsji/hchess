{-# LANGUAGE OverloadedLists #-}

module AI (
    edgeGreed,
    expandHorizon,
    swapForBetter,
    oneStep,
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
import Move (parseMoves)
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
edgeGreed pos depth = edgeGreedCompat pos depth 3

-- silly wrapper, this is not edgeGreed but dig..
edgeGreedCompat :: Position -> Int -> Int -> Either (Position, Status) Position
edgeGreedCompat pos' depth broadness =
    let status = determineStatus pos'
        perspective = toPlay pos'
        evaluated = evaluate' pos' status
        resEither = mkEither $ dig depth broadness (toPlay pos') evaluated
     in case resEither of
            Right poz -> case oneStep pos' poz of
                Just pozz -> Right pozz
                Nothing -> Left (pos', status)
            x -> x
  where
    mkEither :: Evaluated -> Either (Position, Status) Position
    mkEither Evaluated {..} = case status of
        WhiteToPlay -> Right pos
        BlackToPlay -> Right pos
        terminalStatus -> Left (pos, terminalStatus)

-- oneStep-functionality is missing
dig :: Int -> Int -> Color -> Evaluated -> Evaluated
dig depth broadness perspective ev@Evaluated {..} =
    let candidates = positionTree pos
        withStatuses = (\p -> (p, determineStatus p)) <$> candidates
        evaluated = uncurry evaluate' <$> withStatuses
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
            (_, _) -> compare scoreA scoreB
        else case (statusA, statusB) of
            (WhiteIsMate, WhiteIsMate) -> EQ
            (BlackIsMate, BlackIsMate) -> EQ
            (WhiteIsMate, _) -> GT
            (_, WhiteIsMate) -> LT
            (BlackIsMate, _) -> LT
            (_, BlackIsMate) -> GT
            (_, _) -> compare scoreA scoreB

actualEdgeGreed :: Position -> Int -> Either (Position, Status) Position
actualEdgeGreed pos depth =
    let status = determineStatus pos
        color = toPlay pos
        best =
            foldl'
                ( \bestsofar potential ->
                    potential `pseq`
                        bestsofar `par`
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
