{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module FastAI
  ( mkStrategy
  , countEvals
  , mkStrategy'
  , getEval
  ) where

import           Chess
import           Control.DeepSeq
import           GHC.Generics    (Generic, Generic1)
import           Data.Functor
import           Evaluation

-- span in evaluation values
type Outlook = (Float, Float)

-- represents a single strategy tree based on a potential evaluated position
data Strategy =
  Strategy Evaluated
           [Strategy]
  deriving (Eq, Generic, NFData)

mkStrategy :: Int -> GameHistory -> Strategy
mkStrategy 0 gh = Strategy (evaluate' gh) []
mkStrategy depth gh =
  let nextPositions = positionTree' gh
      evaluated = evaluate' gh
      nextDepth = depth - 1
      nextStrategies = mkStrategy nextDepth <$> nextPositions
   --in Strategy evaluated $! force nextStrategies
   in Strategy evaluated nextStrategies

countEvals :: Strategy -> Integer
countEvals (Strategy eval []) = 1
countEvals (Strategy eval x)  = 1 + sum (fmap countEvals x)

mkStrategy' :: GameHistory -> Int -> (Strategy, Integer)
mkStrategy' gh depth =
  let strat = mkStrategy depth gh
      count = countEvals strat
   in (strat, count)

eval :: Strategy -> Evaluated
eval (Strategy e _) = e

getEval :: Strategy -> [Int] -> Maybe Evaluated
getEval (Strategy e _) [] = Just e
getEval (Strategy _ strats) (idx:idxs) =
  if idx < length strats
    then getEval (strats !! idx) idxs
    else Nothing
