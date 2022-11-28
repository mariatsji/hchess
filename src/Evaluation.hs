module Evaluation (
    Evaluated (..),
    evaluate,
    getPosition,
    evaluate',
) where

import Chess
import Control.Parallel.Strategies (NFData)
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
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
    foldl'
        ( \acc mP ->
            let val = maybe 0 valueOf mP
             in acc `seq` acc + val
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
