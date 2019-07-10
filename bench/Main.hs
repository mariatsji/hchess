import qualified AI                            as AI
import qualified Chess                         as Chess
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types
import qualified Evaluation                    as Evaluation
import qualified Printer                       as Printer

import           Control.Monad
import           Data.Maybe

benchConf :: Config
benchConf = defaultConfig { timeLimit = 10, reportFile = Just "criterion.report" }

main =
  defaultMainWith benchConf --[bench "asdf" $ whnf (1 +) 1]
    [ moveBench,
    aiBench
    ]

moveBench = bgroup
  "moves"
  [ bench "positionTree startPosition"
      $ nf Chess.positionTree Chess.startPosition
  ]

aiBench = bgroup
  "chess"
  [ bench "expandHorizon 1 from startPosition"
    $ nf (AI.expandHorizon 1) Chess.startPosition
  -- , bench "streamBest depth 2 from startPosition"
  --  $ whnf (AI.streamBest Chess.startPosition) 2
  ]
