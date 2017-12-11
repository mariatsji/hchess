import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

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
        it "behaves ok using AI.best after second move for white" $ do
            let moves = ["e2-e4", "b8-a6", "d2-d4"]
            let p = Move.parseMoves moves
            let b = AI.best p 2
            case b of Right gh -> do
                        Printer.pretty $ head gh
                      Left status -> do
                        print "bad, test should fail"
        it "searches 10^n moves from startposition with focused" $ do
            let t = AI.focused' (evaluate' [startPosition]) 4
            length t `shouldSatisfy` (>9000)
            length (toGH $ t !! 3000) `shouldBe` (length (toGH $ t !! 4000))
            -- prettyGH ((snd . toGH) $ t !! 5000)
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
        --it "behaves ok using AI.focusedBest after second move for white" $ do
        --    let moves = ["e2-e4", "b8-a6", "d2-d4"]
        --    let p = Move.parseMoves moves
        --    let b = AI.focusedBest p 2
        --    case b of Right gh -> do
        --                Printer.pretty $ head gh
        --              Left status -> do
        --                print "bad, test should fail"