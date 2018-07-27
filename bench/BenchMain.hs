import Criterion.Main
import qualified Chess as Chess

main = defaultMain [
    bgroup "chess" [ bench "positionTree' startposition" $ whnf Chess.positionTree' [Chess.startPosition] ] 
  ]
