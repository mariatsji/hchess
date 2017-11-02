import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Chess
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
        it "finds 16 opening white pawn moves" $ do
            let p = Chess.whitePawnMovesNaive Chess.startPosition
            length p `shouldBe` (16 :: Int)
        it "finds 16 opening knight moves even though some are outside board" $ do
            let k = Chess.whiteKnightMovesNaive Chess.startPosition
            length k `shouldBe`(16 :: Int)