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
    Position (Position, gamehistory, m, toPlay),
    findMove,
    mkPositionExpensive,
    next,
 )
import Printer (prettyE)
import qualified Debug.Trace as Debug

-- restore edgeGreed until oneStep is in place
edgeGreed :: Position -> Int -> Either (Position, Status) Position
edgeGreed pos depth = edgeGreedCompat pos depth 200

-- silly wrapper, this is not edgeGreed but dig..
edgeGreedCompat :: Position -> Int -> Int -> Either (Position, Status) Position
edgeGreedCompat pos' depth broadness =
    let perspective = toPlay pos'
        evaluated = evaluate' pos'
        resEither = filterOnlyPlayable $ dig depth broadness (toPlay pos') evaluated
     in case resEither of
            Right ev ->
                case oneStep pos' (pos ev) of
                    Left s -> Debug.trace s $ Left (pos ev, status ev)
                    Right newPos -> Right newPos
            x -> fmap pos x
  where
    filterOnlyPlayable :: Evaluated -> Either (Position, Status) Evaluated
    filterOnlyPlayable ev@Evaluated {..} = case status of
        WhiteToPlay -> Right ev
        BlackToPlay -> Right ev
        terminalStatus -> Left (pos, terminalStatus)

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

-- par/pseq fmap
paraMap :: (a -> b) -> [a] -> [b]
paraMap f [] = []
paraMap f (x : xs) =
    let a = f x
        as = paraMap f xs
     in a `pseq` as `par` a : as

(<-$->) = paraMap
infixl 4 <-$->

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

-- todo this throws away sooooo much info!
oneStep :: Position -> Position -> Either String Position
oneStep p (Position snpAdvanced ghAdvanced _ _ _) =
    let snp = m p
        gh = gamehistory p
        idx = (length ghAdvanced - length gh) - 1
     in case safeLookup idx (snpAdvanced : ghAdvanced) of
            Nothing -> Left $ "Could not lookup oneStep, from gh length " <> show (length gh) <> " to length advanced " <> show (length ghAdvanced)
            Just nextSnp -> Right $ mkPositionExpensive p nextSnp

safeLookup :: Int -> [a] -> Maybe a
safeLookup _ [] = Nothing
safeLookup 0 (x : _) = Just x
safeLookup i (_ : xs) = safeLookup (i - 1) xs
