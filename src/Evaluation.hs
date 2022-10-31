module Evaluation (
    Evaluated (..),
    evaluate,
    getPosition,
    evaluate',
) where

import Chess
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Position
import Control.Parallel (par)
import GHC.Conc (pseq)

data Evaluated
    = Evaluated
        { pos :: Position
        , score :: Float
        , status :: Status }
    deriving stock (Eq, Show)

getPosition :: Evaluated -> Position
getPosition (Evaluated p _ _) = p

evaluate' :: Position -> Status -> Evaluated
evaluate' gh status = case status of
    WhiteIsMate -> Evaluated gh (-10000.0) WhiteIsMate
    BlackIsMate -> Evaluated gh 10000.0 BlackIsMate
    Remis -> Evaluated gh 0 Remis
    playOn -> Evaluated gh (evaluate gh) playOn

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
