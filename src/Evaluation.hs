module Evaluation (
    Evaluated (..),
    evaluate,
    getPosition,
    evaluate',
    terminal,
    deepEval,
) where

import Chess (
    Status (BlackIsMate, Remis, WhiteIsMate),
    determineStatus,
    finalDestinationNotOccupiedBySelf',
    positionTree,
    threefoldrepetition,
    toSquaresBishop',
    toSquaresKnight,
    toSquaresQueen' ,
    toSquaresRook',
    (<-$->),
 )
import Control.DeepSeq (force)
import Control.Parallel (par)
import Control.Parallel.Strategies (NFData)
import Data.Maybe (fromMaybe)
import GHC.Conc (pseq)
import GHC.Generics (Generic)
import Position

data Evaluated = Evaluated
    { pos :: Position
    , score :: Float
    , status :: Status
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (NFData)

getPosition :: Evaluated -> Position
getPosition (Evaluated p _ _) = p

deepEval :: Int -> Color -> Position -> Float
deepEval depth perspective pos =
    let status = determineStatus pos
        candidates = positionTree pos
        evaluated = evaluate <-$-> candidates
     in if terminal status
            then score $ evaluate' pos
            else
                if depth == 0
                    then
                        fromMaybe (error "Not terminal status, so there should be candidates at depth 0") $
                            singleBest' perspective evaluated
                    else
                        fromMaybe (error "Not terminal status, so there should be candidates at depth > 0") $
                            singleBest' perspective $
                                deepEval (depth - 1) (next perspective) <$> candidates

terminal :: Status -> Bool
terminal = \case
    WhiteIsMate -> True
    BlackIsMate -> True
    Remis -> True
    _ -> False

singleBest' :: Color -> [Float] -> Maybe Float
singleBest' _ [] = Nothing
singleBest' White fs = Just $ maximum fs
singleBest' Black fs = Just $ minimum fs

evaluate' :: Position -> Evaluated
evaluate' pos =
    case determineStatus pos of
        WhiteIsMate -> Evaluated pos (-10000.0) WhiteIsMate
        BlackIsMate -> Evaluated pos 10000.0 BlackIsMate
        Remis -> Evaluated pos 0 Remis
        playOn -> Evaluated pos (evaluate pos) playOn

-- CAF good?
evaluate :: Position -> Float
evaluate p =
    if threefoldrepetition p
        then 0.0
        else
            foldr
                ( \(s, mP) acc ->
                    case mP of
                        Nothing -> acc
                        Just pie ->
                            let pieceVal = force $ valueOf pie
                                impactVal = force $ impactArea (m p) pie s
                             in pieceVal `par` impactVal `pseq` acc + pieceVal + impactVal
                )
                0
                (toList' (m p))

valueOf :: Piece -> Float
valueOf (Pawn c) = colorFactor c * 1.0
valueOf (Knight c) = colorFactor c * 3.0
valueOf (Bishop c) = colorFactor c * 3.0
valueOf (Rook c) = colorFactor c * 5.0
valueOf (Queen c) = colorFactor c * 9.0
valueOf (King c) = colorFactor c * 100.0

colorFactor :: Color -> Float
colorFactor c = if c == Black then (-1) else 1

impactArea :: Snapshot -> Piece -> Square -> Float
impactArea _ (Pawn White) (Square _ r) = 0.005 * fromIntegral r
impactArea _ (Pawn Black) (Square _ r) = (-0.005) * fromIntegral ( 9 - r ) 
impactArea _ (King _) _ = 0
impactArea snp (Knight c) s =
    let reachables = fromIntegral . length $ filter (finalDestinationNotOccupiedBySelf' snp c) (toSquaresKnight s)
     in colorFactor c * reachables * 0.01
impactArea snp (Bishop c) s =
    let reachables = fromIntegral . length $ toSquaresBishop' snp c s
     in colorFactor c * reachables * 0.01
impactArea snp (Rook c) s =
    let reachables = fromIntegral . length $ toSquaresRook' snp c s
     in colorFactor c * reachables * 0.01
impactArea snp (Queen c) s =
    let reachables = fromIntegral . length $ toSquaresQueen' snp c s
     in colorFactor c * reachables * 0.01