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
            let tree = Chess.positionTree [Chess.startPosition]
            mapM_ Printer.pretty tree
            length tree  `shouldBe` (20 :: Int)
        it "parses a move text command" $ do
            let newP = Move.parseMove "e2-e4" Chess.startPosition
            length newP `shouldBe` (64:: Int)
            newP `shouldNotBe` Chess.startPosition
            Printer.pretty newP
        it "does not step on own pieces" $ do
            let b = Chess.canGoThere Chess.startPosition ('a',1) ('a', 2)
            b `shouldBe` (False :: Bool)
        it "knows when destination square is occupied by own color" $ do
            let b = Chess.finalDestinationNotOccupiedBySelf Chess.startPosition ('a', 1) ('a', 2)
            b `shouldBe` (False :: Bool)
        it "finds the correct traversed numeric squares in a straight bishop-like move" $ do
            let squares = Chess.points' (3,3) (5,5)
            squares `shouldBe` ([(4,4)] :: [(Int, Int)])
        it "finds the correct traversed numeric squares in a straight rook-like move" $ do
            let squares = Chess.points' (3,3) (6,3)
            squares `shouldBe` ([(4,3), (5,3)] :: [(Int, Int)])
        it "finds the correct traversed squares in a straight bishop-like move" $ do
            let squares = Chess.points ('e', 3) ('g', 5)
            squares `shouldBe` ([('f', 4)] :: [Square])
        it "finds the correct traversed squares in a straight rook-like move" $ do
            let squares = Chess.points ('a',1) ('a', 4)
            squares `shouldBe` ([('a',2),('a',3)] :: [Square])