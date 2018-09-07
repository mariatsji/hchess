import qualified AI             as AI
import qualified Chess          as Chess
import           Criterion.Main
import qualified Evaluation     as Evaluation
import qualified FastAI         as FastAI
import qualified Printer        as Printer

import           Control.Monad
import           Data.Maybe

main = do
  defaultMain
    [ 
      -- fastAIBench
    legacyBench
    ]


fastAIBench =
  bgroup
    "fast ai"
    [ {--
      bench "mkStrategy startposition depth 0" $
      nf (FastAI.mkStrategy' [Chess.startPosition]) 0
    , bench "mkStrategy startposition depth 1" $
      nf (FastAI.mkStrategy' [Chess.startPosition]) 1
    , bench "mkStrategy startposition depth 2" $
      nf (FastAI.mkStrategy' [Chess.startPosition]) 2
    , bench "mkStrategy startposition depth 3" $
      nf (FastAI.mkStrategy' [Chess.startPosition]) 3
      --}
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

someCounting = do
  let depths = take 6 [0 ..]
  forM_
    depths
    (\i -> do
       let d = FastAI.mkStrategy i [Chess.startPosition]
           count = FastAI.countEvals d
       putStrLn $! "Depth " ++ (show i) ++ ": #" ++ (show count))
