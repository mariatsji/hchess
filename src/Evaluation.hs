module Evaluation
  ( Evaluated (..),
    evaluate,
    getPosition,
    evaluate',
    terminal,
    deepEval,
    alphaBeta,
    centralization,
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
  let material = sum $ fmap (maybe 0 valueOf) snp
   in material + centralization snp

valueOf :: Piece -> Float
valueOf (Pawn c) = colorFactor c * 1.0
valueOf (Knight c) = colorFactor c * 3.0
valueOf (Bishop c) = colorFactor c * 3.0
valueOf (Rook c) = colorFactor c * 5.0
valueOf (Queen c) = colorFactor c * 9.0
valueOf (King c) = colorFactor c * 100.0

colorFactor :: Color -> Float
colorFactor c = if c == Black then (-1) else 1

-- | Centralization bonus between -0.99 and +0.99.
-- Pieces closer to the center score higher. Kings are excluded.
centralization :: Snapshot -> Float
centralization snp =
  let pieces = mapMaybe (\(w, mp) -> (unHash w,) <$> mp) $ toList' snp
      total = sum [colorFactor (colr p) * centralWeight p s | (s, p) <- pieces]
      maxPossible = 16 * 0.12 -- theoretical max (all 16 pieces in center)
   in clamp $ total / maxPossible
  where
    centralWeight (King _) _ = 0
    centralWeight _ (Square c r) =
      let cDist = abs (fromIntegral c - 4.5 :: Float)
          rDist = abs (fromIntegral r - 4.5 :: Float)
       in max 0 (2.0 - cDist) * max 0 (2.0 - rDist) * 0.04
    clamp x = max (-0.99) (min 0.99 x)
