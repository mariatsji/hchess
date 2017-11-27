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
            Chess.pieceAt newPos ('e', 4) `shouldBe` (Just $ Pawn White :: Maybe Piece)
        it "finds 16 white pieces in startpos" $ do
            length (Chess.whitePieces Chess.startPosition) `shouldBe` (16 :: Int)
        it "finds 16 black pieces in startpos" $ do
            length (Chess.blackPieces Chess.startPosition) `shouldBe` (16 :: Int)

    describe "Move" $ do
        it "finds 20 possible opening moves for white" $ do
            let tree = Chess.positionTree [Chess.startPosition]
            length tree  `shouldBe` (20 :: Int)
        it "parses a move text command" $ do
            let newP = head $ Move.parseMove "e2-e4" [Chess.startPosition]
            length newP `shouldBe` (64:: Int)
            newP `shouldNotBe` Chess.startPosition
        it "does not step on own pieces" $ do
            let b = Chess.canGoThere Chess.startPosition ('a',1) ('a', 2)
            b `shouldBe` (False :: Bool)
        it "lets pawns move ahead from startpos" $ do
            let b = Chess.canGoThere Chess.startPosition ('e',2) ('e',4)
            b `shouldBe` (True :: Bool)
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
        it "finds the correct traversed squares from h1 - a8" $ do
            let squares = Chess.points ('h', 1) ('a', 8)
            squares `shouldBe` ([('g',2),('f',3),('e',4),('d',5),('c',6),('b',7)] :: [Square])
        it "finds toSquares for pawns in startrow" $ do
            let squares = Chess.toSquaresPawn Chess.startPosition (('e', 2), Pawn White)
            squares `shouldMatchList` ([('e',3),('e',4)] :: [Square])
        it "recognizes a position with a king" $ do
            let b = Chess.anyPosWithoutKing White [Chess.startPosition]
            b `shouldBe` (False :: Bool)
        it "recognizes a position without a king" $ do
            let b = Chess.anyPosWithoutKing White [[(('a',1), Just $ Bishop White)]]
            b `shouldBe` (True :: Bool)
        it "finds a small number of end-positions" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('h', 7) (Pawn White)
            let t = Chess.positionTreeIgnoreCheck [p3, p3] -- fake blacks turn
            length t `shouldBe` (3 :: Int) 
        it "knows that white is in check" $ do
            let p1 = Move.parseMove "e2-e4" [Chess.startPosition]
            let p2 = Move.parseMove "d7-d5" p1
            let p3 = Move.parseMove "e4-d5" p2
            let p4 = Move.parseMove "d8-d5" p3
            let p5 = Move.parseMove "h2-h4" p4
            let p6 = Move.parseMove "d5-e5" p5
            Chess.isInCheck (head p6) (toPlay p6) `shouldBe` (True :: Bool)
        it "knows that white is not in check" $ do
            let p1 = Move.parseMove "e2-e4" [Chess.startPosition]
            let p2 = Move.parseMove "d7-d5" p1
            let p3 = Move.parseMove "e4-d5" p2
            let p4 = Move.parseMove "d8-d5" p3
            let p5 = Move.parseMove "h2-h4" p4
            let p6 = Move.parseMove "d5-a5" p5
            Chess.isInCheck (head p6) (toPlay p6) `shouldBe` (False :: Bool)
        it "knows all next positions where not in check" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('h', 7) (Pawn White)
            let p4 = Chess.replacePieceAt p3 ('g', 8) (Rook White)
            let t = Chess.positionTree [p4, p3]
            length t `shouldBe` (1 :: Int)
        it "knows that black is check mate" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('h', 7) (Pawn White)
            let p4 = Chess.replacePieceAt p3 ('g', 8) (Queen White)
            let t = Chess.positionTree [p4, p4]
            Chess.isCheckMate [p4, p4] `shouldBe` (True :: Bool)
        it "knows that position is stalemate" $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 8) (King Black)
            let p2 = Chess.replacePieceAt p1 ('e', 1) (King White)
            let p3 = Chess.replacePieceAt p2 ('h', 6) (Pawn White)
            let p4 = Chess.replacePieceAt p3 ('g', 6) (Pawn White)
            let p5 = Chess.replacePieceAt p4 ('h', 7) (Pawn White)
            let t = Chess.positionTree [p5, p4]
            Chess.isPatt [p5, p4] `shouldBe` (True :: Bool)
        it "promotes pawns for Black " $ do
            let p1 = Chess.replacePieceAt Chess.emptyBoard ('h', 1) (Pawn Black)
            let p2 = Chess.promote Black p1
            Chess.pieceAt (head p2) ('h',1)`shouldBe` (Just (Queen Black))
            Chess.pieceAt (head $ tail p2) ('h',1)`shouldBe` (Just (Rook Black))
            Chess.pieceAt (head $ tail $ tail p2) ('h',1)`shouldBe` (Just (Bishop Black))
            Chess.pieceAt (last $ p2) ('h',1)`shouldBe` (Just (Knight Black))