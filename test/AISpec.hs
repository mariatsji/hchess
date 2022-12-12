module AISpec where

import AI
import Chess
import Evaluation
import Move
import Position
import Test.Hspec

spec :: Spec
spec = describe "AI" $ do
    it "evaluates the start position as reasonably balanced" $ do
        let e = Evaluation.evaluate startPosition
        e `shouldSatisfy` (> (-1.1))
        e `shouldSatisfy` (< 1.1)
    it "digs up a response to a mate threat" $ do
        let Right pos = playMoves ["e2-e4", "a7-a5", "f1-c4", "a5-a4", "d1-h5"]
            Right best = AI.edgeGreed pos 1
        length (gamehistory best) `shouldBe` 6
