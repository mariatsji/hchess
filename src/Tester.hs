module Tester where

import qualified AI
import Chess
import Move
import Position
import Printer
import Evaluation
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)

test :: IO ()
test = do
    let perspective = Black
        Right pos' = playMoves ["e2-e4", "f7-f5", "d1-g4"]
    print $
        deepEval 3 Black pos'