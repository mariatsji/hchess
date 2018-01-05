import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import PGN

main :: IO ()
main = hspec $ do
    describe "PGN" $ do
        it "allows the appending of moves to Pgn" $ do
            let pgn = PGN.appendMove [] "e4"
            length pgn `shouldBe` (1 :: Int)
            head pgn `shouldBe` (WhiteMove 1 "e4")
        it "allows a black reply to be appended to Pgn" $ do
            let pgn = PGN.appendMove [] "e4"
            let replyPgn = PGN.appendMove pgn "e5"
            length replyPgn `shouldBe` (1 :: Int)
            head replyPgn `shouldBe` (BlackMove 1 "e4" "e5")
        it "allows white reply with his second move to Pgn" $ do
            let pgn = PGN.appendMove [] "e4"
            let replyPgn = PGN.appendMove pgn "e5"
            let replyReplyPgn = PGN.appendMove replyPgn "Nf3"
            length replyReplyPgn `shouldBe` (2 :: Int)
            head replyReplyPgn `shouldBe` (WhiteMove 2 "Nf3")