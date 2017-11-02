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

