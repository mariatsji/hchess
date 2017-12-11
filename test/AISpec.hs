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
        it "finds unique paths in positionTreeSearch" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('a', 6) (Rook White)
            let p4 = Chess.replacePieceAt p3 ('b', 5) (Rook White)
            let gh = [p4]
            let t = AI.positionTreeSearch gh 2
            length t `shouldSatisfy` (> 30)
            any (== (head t)) (tail t) `shouldBe` False
        it "finds a mate in 1 moves with search depth 2" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('a', 7) (Rook White)
            let p4 = Chess.replacePieceAt p3 ('b', 6) (Rook White)
            let gh = [p4]
            let t = AI.bestSearchedGH gh 2
            (\(a,b,c) -> c) t `shouldBe` BlackIsMate
        it "selects best move for white with focused search" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('f', 8) (Queen Black)
            let p4 = Chess.replacePieceAt p3 ('e', 7) (Pawn White)
            let gh = [p4]
            let f = focused gh 2

            let b = focusedBest gh 2
            case b of Right gh -> do
                        Printer.pretty $ head gh
                        pieceAt (head gh) ('f', 8) `shouldBe` (Just (Queen White))
                      Left status -> do
                         print "bad, test, should fail"
        it "behaves ok using AI.best after second move for white" $ do
            let moves = ["e2-e4", "b8-a6", "d2-d4"]
            let p = Move.parseMoves moves
            let b = AI.best p 2
            case b of Right gh -> do
                        Printer.pretty $ head gh
                      Left status -> do
                        print "bad, test should fail"
        it "finds a nice move from startposition with focused" $ do
            let t = AI.focused [startPosition] 4
            prettyGH ((snd . toGH) t)
        it "finds a nice move when few pieces on board" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('e', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('a', 7) (Queen White)
            let p4 = Chess.replacePieceAt p3 ('e', 7) (Knight Black)
            let p5 = Chess.replacePieceAt p4 ('h', 8) (Rook Black)
            let gh = [p5]
            let t = AI.focused gh 4
            prettyGH ((snd . toGH) t)
        it "doesnt mind focused search some moves in to the game" $ do
            let moves = ["e2-e4", "e7-e5", "d2-d4", "d7-d5", "g1-f3", "g8-f6", "b1-c3", "b8-c6", "f1-e3", "f8-e7", "g2-g3", "g7-g5", "f1-g2", "O-O", "O-O", "c8-g4", "d1-d3", "g4-f3", "g2-f3"]
            let ghDeep = Move.parseMoves moves
            let ghStart = [Chess.startPosition]
            print (Chess.positionTree' ghDeep)
