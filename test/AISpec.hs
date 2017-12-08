import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Chess
import Move
import Printer
import AI

main :: IO ()
main = hspec $ do
    describe "AI" $ do
        it "evaluates the start position as reasonably balanced" $ do
            let e = AI.evaluate Chess.startPosition
            e `shouldSatisfy` (> (-1.1))
            e `shouldSatisfy` (< 1.1)
        it "evaluate advanced pawns over home pawns" $ do
            let p = Chess.makeMoves [Chess.startPosition] [ (('e', 2), ('e', 4)) ]
            let e = AI.evaluate $ head p
            e `shouldSatisfy` (> 0.0)
        it "finds unique paths in positionTreeSearch" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('a', 6) (Rook White)
            let p4 = Chess.replacePieceAt p3 ('b', 5) (Rook White)
            let gh = [p4]
            let t = AI.positionTreeSearch gh 4
            (length t) `shouldSatisfy` (> 1000)
            any (== (head t)) (tail t) `shouldBe` False
        it "finds a mate in 1 moves with search depth 2" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('a', 7) (Rook White)
            let p4 = Chess.replacePieceAt p3 ('b', 6) (Rook White)
            let gh = [p4]
            let t = AI.bestSearchedGH gh 2
            (\(a,b,c) -> c) t `shouldBe` BlackIsMate
        it "behaves ok after second move for white" $ do
            let moves = ["e2-e4", "b8-a6", "d2-d4"]
            let p = Move.parseMoves moves
            Printer.pretty $ head p
            let b = AI.best p 3
            case b of Right gh -> do
                        Printer.pretty $ head gh
                      Left status -> do
                        print "bad, test should fail"