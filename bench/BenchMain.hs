import qualified AI             as AI
import qualified Chess          as Chess
import           Criterion.Main
import qualified Evaluation     as Evaluation
import qualified Printer        as Printer

import           Control.Monad
import           Data.Maybe

main = do
  defaultMain
    [ 
    legacyBench
    ]


legacyBench =
  bgroup
    "chess"
    [ bench "positionTree' startposition" $
      nf Chess.positionTree' [Chess.startPosition]
    , bench "evaluation' startposition" $
      nf Evaluation.evaluate' [Chess.startPosition]
      {--
    , bench "focusedBest startposition depth 1" $
      whnf (AI.focusedBest [Chess.startPosition]) 1
    , bench "focusedBest startposition depth 3" $
      whnf (AI.focusedBest [Chess.startPosition]) 3
    , bench "focusedBest startposition depth 5" $
      whnf (AI.focusedBest [Chess.startPosition]) 5
      --}
    ]
