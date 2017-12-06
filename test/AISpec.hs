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
        it "makes a best move with depth 1" $ do
            let p = Chess.makeMoves [Chess.startPosition] [ (('e', 2), ('e', 4)) ]
            let t = AI.firstBest p
            length t `shouldBe` (3 :: Int)
        it "searches the position tree to given depth " $ do
            let t = AI.positionTreeSearch [Chess.startPosition]
            length t `shouldBe` (500 :: Int)
        it "finds a mate in 2 moves with search depth 5" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('a', 6) (Rook White)
            let p4 = Chess.replacePieceAt p3 ('b', 5) (Rook White)
            let x = AI.bestSearchedPosition [p4, p3, p2, p1, Chess.emptyBoard]
            length x `shouldBe` (1 :: Int)