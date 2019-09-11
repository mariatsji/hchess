import qualified AI
import qualified Chess
import Control.Monad
import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import Data.Maybe
import qualified Evaluation
import qualified Printer

benchConf :: Config
benchConf = defaultConfig {timeLimit = 10, reportFile = Just "criterion.report"}

main =
  defaultMainWith benchConf --[bench "asdf" $ whnf (1 +) 1]
    [ moveBench,
      aiBench
      ]

moveBench =
  bgroup
    "moves"
    [ bench "positionTree startPosition"
        $ nf Chess.positionTree startPosition
      ]

aiBench =
  bgroup
    "chess"
    [ bench "expandHorizon 1 from startPosition"
        $ nf (AI.expandHorizon 1) startPosition
      ]
-- , bench "streamBest depth 2 from startPosition"
--  $ whnf (AI.streamBest startPosition) 2
