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
    it "successfully steps one point in a direction with oneStep with 1 look a head" $ do
        let p1 = startPosition
        let p2 = head $ positionTree p1
        let os = oneStep p1 p2
        oneStep p1 p2 `shouldBe` p2
    it "successfully steps one point in a direction with oneStep based on 2 looks ahead" $ do
        let p1 = startPosition
        let p2 = head $ positionTree p1
        let p3 = head $ positionTree p2
        oneStep p1 p3 `shouldBe` p2
    it "checks a rather specific oneStep function" $ do
        let Right p = playMoves ["e2-e4", "d7-d5", "e4-d5"]
        let Right best = AI.edgeGreed p 2
        m best `shouldNotBe` startTree
    it "sorts a small positionTree for black" $ do
        let Right p = playMoves ["e2-e4", "g7-g6", "d1-h5"]
            evaluated = evaluate' <$> positionTree p
            [queenTake] = AI.best 1 Black evaluated -- it should take the queen here
        pieceAt (pos queenTake) (Square 8 5) `shouldBe` Just (Pawn Black) -- took the queen
    it "sorts a small positionTree for white" $ do
        let Right p = playMoves ["e2-e4", "f7-f5"]
            evaluated = evaluate' <$> positionTree p
            [queenTake] = AI.best 1 White evaluated -- it should take the pawn here
        pieceAt (pos queenTake) (Square 6 5) `shouldBe` Just (Pawn White) -- took the pawn
    it "digs up a response to a mate threat" $ do
        let Right pos = playMoves ["e2-e4", "a7-a5", "f1-c4", "a5-a4", "d1-h5"]
            Right best = AI.edgeGreed pos 1
        length (gamehistory best) `shouldBe` 6
