import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Chess

main :: IO ()
main = hspec $ do
    describe "Chess.board" $ do
        it "creates a board with 64 squares" $ do
            print Chess.board
            length Chess.board `shouldBe` (64 :: Int)

