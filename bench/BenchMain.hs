import qualified AI                            as AI
import qualified Chess                         as Chess
import           Criterion.Main
import qualified Evaluation                    as Evaluation
import qualified Printer                       as Printer

import           Control.Monad
import           Data.Maybe

main = do
  defaultMain [moveBench, aiBench]

moveBench = bgroup
  "moves"
  [ bench "positionTree startPosition"
      $ whnf Chess.positionTree Chess.startPosition
  ]

aiBench = bgroup
  "chess"
  [ bench "expandHorizon 2 from startPosition"
    $ whnf (AI.expandHorizon 2) Chess.startPosition
  , bench "streamBest depth 2 from startPosition"
    $ whnf (AI.streamBest Chess.startPosition) 2
  ]
