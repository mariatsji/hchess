module Evaluation (
    Evaluated (..),
    evaluate,
    getPosition,
    evaluate',
    terminal,
    deepEval
) where

import Chess
import Control.Parallel.Strategies (NFData)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Position
import Control.Parallel (par)
import GHC.Conc (pseq)
import Control.DeepSeq (force)

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
deepEval depth perspective pos  =
    let status = determineStatus pos
        candidates = positionTree pos
        evaluated = evaluate <-$-> candidates
     in
    if terminal status then score $ evaluate' pos
    else if depth == 0 then
         fromMaybe (error "Not terminal status, so there should be candidates at depth 0") $
            singleBest' perspective evaluated
    else
        fromMaybe  (error "") $
            singleBest' perspective $ deepEval (depth - 1) (next perspective) <-$-> candidates

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
    foldr
        ( \mP acc ->
            let val = force $ maybe 0 valueOf mP
             in 
                --force acc `par` val `par` acc `pseq` acc + val
                acc + val
        )
        0
        (m p)

valueOf :: Piece -> Float
valueOf (Pawn c) = (if c == Black then (-1) else 1) * 1.0
valueOf (Knight c) = (if c == Black then (-1) else 1) * 3.0
valueOf (Bishop c) = (if c == Black then (-1) else 1) * 3.0
valueOf (Rook c) = (if c == Black then (-1) else 1) * 5.0
valueOf (Queen c) = (if c == Black then (-1) else 1) * 9.0
valueOf (King c) = (if c == Black then (-1) else 1) * 100.0
