import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Chess
import Move
import Printer

main :: IO ()
main = hspec $ do
    describe "Chess.board" $ do
        it "prints the start position" $ do
            Printer.pretty Chess.startPosition
        it "creates a board with 64 squares" $ do
            length Chess.board `shouldBe` (64 :: Int)
        it "moves E2-E4 from start pos" $ do
            let newPos = Chess.movePiece Chess.startPosition ('e',2) ('e',4)
            Printer.pretty newPos
        it "finds 16 white pieces in startpos" $ do
            length (Chess.whitePieces Chess.startPosition) `shouldBe` (16 :: Int)
        it "finds 16 black pieces in startpos" $ do
            length (Chess.blackPieces Chess.startPosition) `shouldBe` (16 :: Int)

    describe "Move" $ do
        it "finds 20 possible opening moves for white" $ do
            let tree = (Chess.positionTree Chess.startPosition White)
            mapM Printer.pretty tree
            length tree  `shouldBe` (20 :: Int)
        it "parses a move text command" $ do
            let newP = Move.parseMove "e2-e4" Chess.startPosition
            length newP `shouldBe` (64:: Int)
            newP `shouldNotBe` Chess.startPosition
            Printer.pretty newP