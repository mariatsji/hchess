module AISpec where

import AI
import Chess
import Evaluation
import Move
import Position
import Test.Hspec
import Data.Either (isRight)

spec :: Spec
spec = describe "AI" $ do
    it "evaluates the start position as reasonably balanced" $ do
        let e = Evaluation.evaluate startPosition
        e `shouldSatisfy` (> (-1.1))
        e `shouldSatisfy` (< 1.1)
    it "digs up a response to a mate threat" $ do
        let Right pos = playMoves ["e2-e4", "a7-a5", "f1-c4", "a5-a4", "d1-h5"]
            (best, status) = AI.edgeGreed pos 1
        length (gamehistory best) `shouldBe` 6
    it "does not panic in the face of mate" $ do
        let Right pos = playMoves [
                            "e2-e4" ,"a7-a5"
                            ,"d2-d4" ,"e7-e6"
                            ,"g1-f3" ,"b7-b6"
                            ,"f1-c4" ,"c8-a6"
                            ,"c4-a6" ,"a8-a6"
                            ,"O-O" ,"a5-a4"
                            ,"b1-c3" ,"a4-a3"
                            ,"b2-b3" ,"a6-a5"
                            ,"c1-d2" ,"f8-b4"
                            ,"d4-d5" ,"e6-d5"
                            ,"e4-d5" ,"a5-a8"
                            ,"f1-e1" ,"e8-f8"
                            ,"d1-e2" ,"b4-a5"
                            ,"f3-g5" ,"a5-c3"
                            ,"d2-c3" ,"b6-b5"
                            ,"c3-g7" ,"f8-g7"
                            ,"e2-e5" ]
        let (pos, status) = AI.edgeGreed pos 2
        status `shouldBe` BlackToPlay
