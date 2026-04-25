module Evaluation
  ( Evaluated (..),
    evaluate,
    getPosition,
    evaluate',
    terminal,
    deepEval,
    alphaBeta,
  )
where

import Chess
  ( Status (..),
    determineStatus,
    positionTree,
    threefoldrepetition,
    (<-$->),
  )
import Data.List (maximum, minimum)
import Position
import Relude

data Evaluated = Evaluated
  { pos :: Position,
    score :: Float,
    status :: Status
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

getPosition :: Evaluated -> Position
getPosition (Evaluated p _ _) = p

deepEval :: Int -> Color -> Position -> Float
deepEval depth perspective pos =
  let candidates = positionTree pos
      status = determineStatus pos candidates
   in if terminal status
        then score $ evaluate' pos
        else
          fromMaybe (error "Not terminal status, so there should be candidates")
            $ singleBest'
              perspective
            $ if depth == 0
              then evaluate . m <-$-> candidates
              else deepEval (depth - 1) (next perspective) <$> candidates

-- | Negamax with alpha-beta pruning. Much faster than plain minimax (deepEval)
-- because it prunes branches that can't affect the outcome.
alphaBeta :: Int -> Float -> Float -> Color -> Position -> Float
alphaBeta depth alpha beta perspective pos =
  let candidates = positionTree pos
      status = determineStatus pos candidates
   in if depth == 0 || terminal status
        then colorFactor perspective * evaluate (m pos)
        else go candidates alpha
  where
    go [] a = a
    go (p : ps) a =
      let val = -alphaBeta (depth - 1) (-beta) (-a) (next perspective) p
          newA = max a val
       in if newA >= beta
            then newA -- beta cutoff
            else go ps newA

terminal :: Status -> Bool
terminal = flip elem [WhiteIsMate, BlackIsMate, Remis, WhiteResigns, BlackResigns]

singleBest' :: Color -> [Float] -> Maybe Float
singleBest' _ [] = Nothing
singleBest' White (f : fs) = Just $ maximum (f : fs)
singleBest' Black (f : fs) = Just $ minimum (f : fs)

evaluate' :: Position -> Evaluated
evaluate' pos =
  case determineStatus pos (positionTree pos) of
    WhiteIsMate -> Evaluated pos (-10000.0) WhiteIsMate
    BlackIsMate -> Evaluated pos 10000.0 BlackIsMate
    Remis -> Evaluated pos 0 Remis
    playOn ->
      if threefoldrepetition pos
        then Evaluated pos 0.0 Remis
        else Evaluated pos (evaluate (m pos)) playOn

-- much faster evaluate function
evaluate :: Snapshot -> Float
evaluate snp =
  sum
    $ fmap
      (maybe 0 valueOf)
      snp

valueOf :: Piece -> Float
valueOf (Pawn c) = colorFactor c * 1.0
valueOf (Knight c) = colorFactor c * 3.0
valueOf (Bishop c) = colorFactor c * 3.0
valueOf (Rook c) = colorFactor c * 5.0
valueOf (Queen c) = colorFactor c * 9.0
valueOf (King c) = colorFactor c * 100.0

colorFactor :: Color -> Float
colorFactor c = if c == Black then (-1) else 1
