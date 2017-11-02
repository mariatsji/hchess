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
            let whitePawnMoves = Chess.whitePawnMovesTotal Chess.startPosition
            length whitePawnMoves `shouldBe` (16 :: Int)