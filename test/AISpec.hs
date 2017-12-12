import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Data.Typeable

import Chess
import Move
import Evaluation
import Printer
import AI

main :: IO ()
main = hspec $ do
    describe "AI" $ do
        it "evaluates the start position as reasonably balanced" $ do
            let e = Evaluation.evaluate Chess.startPosition
            e `shouldSatisfy` (> (-1.1))
            e `shouldSatisfy` (< 1.1)
        it "evaluate advanced pawns over home pawns" $ do
            let p = Chess.makeMoves [Chess.startPosition] [ (('e', 2), ('e', 4)) ]
            let e = Evaluation.evaluate $ head p
            e `shouldSatisfy` (> 0.0)