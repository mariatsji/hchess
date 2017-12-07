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
            let t = AI.positionTreeSearch gh
            (length t) `shouldSatisfy` (> 1000)
            any (== (head t)) (tail t) `shouldBe` False
        --it "finds a mate in 2 moves with search depth 5" $ do
        --    let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
        --    let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
        --    let p3 = Chess.replacePieceAt p2 ('a', 6) (Rook White)
        --    let p4 = Chess.replacePieceAt p3 ('b', 5) (Rook White)
        --    let gh = [p4]
        --    let t = AI.bestSearchedGH gh
        --    prettyE t
