{-# LANGUAGE OverloadedLists #-}

module AI (
    edgeGreed,
    expandHorizon,
    swapForBetter,
    oneStep,
    test,
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
    Position (Position, toPlay),
    mkPositionExpensive,
 )
import Printer (prettyE)
import qualified Debug.Trace as Debug

dig :: Int -> Color -> Evaluated -> Evaluated
dig depth perspective ev@Evaluated {..} =
    let candidates = positionTree pos
        withStatuses = (\p -> (p, determineStatus p)) <$> candidates
        poses = show (length candidates)
        evaluated = Debug.trace ("positionTree: " <> poses) $ uncurry evaluate' <$> withStatuses
     in if depth == 0
            then Debug.trace "depth 0" $ fromMaybe ev $ singleBest perspective evaluated
            else case find (terminallyGood perspective) evaluated of
                Just x -> Debug.trace "found terminally good" x
                Nothing ->
                    let broadness = 4
                        furtherInspect = best broadness perspective evaluated
                        evLen = show (length evaluated)
                        len = show (length furtherInspect)
                     in Debug.trace ( "Checking " <> len <> " more places (" <> evLen <> ")") $
                        fromMaybe ev $ singleBest perspective $ dig (depth - 1) (next perspective) <$> furtherInspect

singleBest :: Color -> [Evaluated] -> Maybe Evaluated
singleBest color cands = let res = best 1 color cands
    in Debug.trace (show (length res)) listToMaybe res

test :: IO ()
test = do
    let whiteInCheckE = parseMoves ["e2-e4", "d7-d5", "e4-d5", "d8-d5", "h2-h4", "d5-e5"]
    case whiteInCheckE of
        Right whiteInCheck -> do
            let evaled = evaluate' whiteInCheck WhiteToPlay
            prettyE $ dig 1 White evaled
        Left e ->
            print e

terminallyGood :: Color -> Evaluated -> Bool
terminallyGood color Evaluated {..} = case color of
    White -> status == BlackIsMate
    Black -> status == WhiteIsMate

next White = Black
next Black = White

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

edgeGreed :: Position -> Int -> Either (Position, Status) Position
edgeGreed pos depth =
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
