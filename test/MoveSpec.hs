import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

import qualified Data.Map.Lazy    as Map

import           Chess
import           Move
import           Printer

main :: IO ()
main = hspec $ do
    describe "Chess.board" $ do
        it "prints the start position" $
            Printer.pretty Chess.startPosition
        it "creates a board with 64 squares" $
            length Chess.board `shouldBe` (64 :: Int)
        it "moves E2-E4 from start pos" $ do
            let newPos = Chess.movePiece Chess.startPosition (Square 5 2) (Square 5 4)
            Chess.pieceAt newPos (Square 5 4) `shouldBe` (Just $ Pawn White :: Maybe Piece)
        it "finds 16 white pieces in startpos" $
            length (Chess.whitePieces Chess.startPosition) `shouldBe` (16 :: Int)
        it "finds 16 black pieces in startpos" $
            length (Chess.blackPieces Chess.startPosition) `shouldBe` (16 :: Int)

    describe "Move" $ do
        it "finds 20 possible opening moves for white" $ do
            let tree = Chess.positionTree Chess.startPosition
            length tree  `shouldBe` (20 :: Int)
        it "parses a move text command" $ do
            let newP = Move.parseMove "e2-e4" Chess.startPosition
            newP `shouldNotBe` Chess.startPosition
        it "does not step on own pieces" $ do
            let b = Chess.canGoThere Chess.startPosition (Square 1 1) (Square 1  2)
            b `shouldBe` (False :: Bool)
        it "lets pawns move ahead from startpos" $ do
            let b = Chess.canGoThere Chess.startPosition (Square 5 2) (Square 5 4)
            b `shouldBe` (True :: Bool)
        it "knows when destination square is occupied by own color" $ do
            let b = Chess.finalDestinationNotOccupiedBySelf Chess.startPosition (Square 1  1) (Square 1  2)
            b `shouldBe` (False :: Bool)
        it "finds the correct traversed numeric squares in a straight bishop-like move" $ do
            let squares = Chess.points' (3,3) (5,5)
            squares `shouldBe` ([(4,4)] :: [(Int, Int)])
        it "finds the correct traversed numeric squares in a straight rook-like move" $ do
            let squares = Chess.points' (3,3) (6,3)
            squares `shouldBe` ([(4,3), (5,3)] :: [(Int, Int)])
        it "finds the correct traversed squares in a straight bishop-like move" $ do
            let squares = Chess.points (Square 5  3) (Square 7  5)
            squares `shouldBe` ([(Square 6  4)] :: [Square])
        it "finds the correct traversed squares in a straight rook-like move" $ do
            let squares = Chess.points (Square 1 1) (Square 1  4)
            squares `shouldBe` ([(Square 1 2),(Square 1 3)] :: [Square])
        it "finds the correct traversed squares from h1 - a8" $ do
            let squares = Chess.points (Square 8  1) (Square 1  8)
            squares `shouldBe` ([(Square 7 2),(Square 6 3),(Square 5 4),(Square 4 5),(Square 3 6),(Square 2 7)] :: [Square])
        it "finds toSquares for pawns in startrow" $ do
            let squares = Chess.toSquaresPawn Chess.startPosition ((Square 5  2), Pawn White)
            squares `shouldMatchList` ([((Square 5 3), Nothing),((Square 5 4), Nothing)] :: [(Square, Maybe Square)])
        it "recognizes a position with a king" $ do
            let b = Chess.anyPosWithoutKing White [Chess.startPosition]
            b `shouldBe` (False :: Bool)
        it "finds a small number of end-positions" $ do
            let p1 = Chess.replacePieceAt (m Chess.emptyBoard) (Square 8  8) (King Black)
            let p2 = Chess.replacePieceAt p1 (Square 5  1) (King White)
            let p3 = Chess.replacePieceAt p2 (Square 8  7) (Pawn White)
            let t = Chess.positionTreeIgnoreCheck Position { m = p3, gamehistory = [m Chess.emptyBoard] }
            length t `shouldBe` (3 :: Int)
        it "knows that white is in check" $ do
            let p1 = Move.parseMove "e2-e4" Chess.startPosition
            let p2 = Move.parseMove "d7-d5" p1
            let p3 = Move.parseMove "e4-d5" p2
            let p4 = Move.parseMove "d8-d5" p3
            let p5 = Move.parseMove "h2-h4" p4
            let p6 = Move.parseMove "d5-e5" p5
            Chess.isInCheck p6 (toPlay p6) `shouldBe` (True :: Bool)
        it "knows that white is not in check" $ do
            let p1 = Move.parseMove "e2-e4" Chess.startPosition
            let p2 = Move.parseMove "d7-d5" p1
            let p3 = Move.parseMove "e4-d5" p2
            let p4 = Move.parseMove "d8-d5" p3
            let p5 = Move.parseMove "h2-h4" p4
            let p6 = Move.parseMove "d5-a5" p5
            Chess.isInCheck p6 (toPlay p6) `shouldBe` (False :: Bool)
        it "promotes pawns for Black " $ do
            let m1 = Chess.replacePieceAt (m Chess.emptyBoard) (Square 8 1) (Pawn Black)
            let p2 = Chess.promote Black (Position m1 [m Chess.startPosition])
            Chess.pieceAt (head p2) (Square 8 1)`shouldBe` (Just (Queen Black))
            Chess.pieceAt (head $ tail p2) (Square 8 1)`shouldBe` (Just (Rook Black))
            Chess.pieceAt (head $ tail $ tail p2) (Square 8 1)`shouldBe` (Just (Bishop Black))
            Chess.pieceAt (last $ p2) (Square 8 1)`shouldBe` (Just (Knight Black))
        it "finds promotion positions for White" $ do
            let m1 = Chess.replacePieceAt (m Chess.emptyBoard) (Square 5 8) (Pawn White)
            let t = Chess.promoteBindFriendly White (Position m1 [])
            length t `shouldBe` (4 :: Int)
        it "leaves unpromotable boards alone for White" $ do
            let m1 = Chess.replacePieceAt (m Chess.emptyBoard) (Square 5  7) (Pawn White)
            let t = Chess.promoteBindFriendly White (Position m1 [])
            t `shouldBe` [Position m1 []]
        it "promotes passed pawns for Black in the position tree" $ do
            let m1 = Chess.replacePieceAt (m Chess.emptyBoard) (Square 5  2) (Pawn Black)
                p1 = Position m1 [m Chess.startPosition]
            let t = Chess.positionTree p1
            Chess.pieceAt (head t) (Square 5  1) `shouldBe` (Just (Queen Black))
            length t `shouldBe` (4 :: Int)
        it "does a long castle for black when the startpos is used" $ do
            let m1 = Chess.removePieceAt (m Chess.startPosition) (Square 2  8)
                m2 = Chess.removePieceAt m1 (Square 3  8)
                m3 = Chess.removePieceAt m2 (Square 4  8)
                p = Position m3 [m2, m1, m Chess.startPosition]
                c = Chess.castleLong p Black
            length c `shouldBe` (1 :: Int)
            Chess.pieceAt (head c) (Square 3  8) `shouldBe` (Just (King Black))
            Chess.pieceAt (head c) (Square 4  8) `shouldBe` (Just (Rook Black))
        it "includes long castle for white in legal moves" $ do
            let moves = ["d2-d4", "d7-d5", "b1-c3", "e7-e5", "b2-b3", "f7-f5", "c1-b2", "g7-g5", "d1-d2", "h7-h5"]
            let gh = Move.parseMoves moves
            let legals = Chess.positionTree gh
            let kingMoves = filter (\p -> pieceAt p (Square 5  1) == Nothing) legals
            length kingMoves `shouldBe` (2 :: Int)
        it "parses a long castle for white" $ do
            let moves = ["d2-d4", "d7-d5", "b1-c3", "e7-e5", "b2-b3", "f7-f5", "c1-b2", "g7-g5", "d1-d2", "h7-h5"]
                gh = Move.parseMoves moves
                p2 = Move.parseMove "O-O-O" gh
            length (gamehistory p2) - (length (gamehistory gh)) `shouldBe` (1 :: Int)
        it "white does not castle through check" $ do
            let p = Chess.makeMoves Chess.startPosition [ ((Square 5  2), (Square 5  4))
                  , ((Square 5  7), (Square 5  5))
                  , ((Square 7  1), (Square 6  3))
                  , ((Square 2  8), (Square 3  6))
                  , ((Square 6  1), (Square 2  5))
                  , ((Square 4  7), (Square 4  6))
                  , ((Square 2  1), (Square 3  3))
                  , ((Square 4  8), (Square 7  5))
                  , ((Square 3  3), (Square 4  5))
                  , ((Square 7  5), (Square 7  2)) ]
            let p2 = Chess.castleShort p White
            p2 `shouldBe` []
        it "lets white castle from moves out of the opening" $ do
            let p = Chess.makeMoves Chess.startPosition [ ((Square 5  2), (Square 5  4))
                  , ((Square 5  7), (Square 5  5))
                  , ((Square 7  1), (Square 6  3))
                  , ((Square 2  8), (Square 3  6))
                  , ((Square 6  1), (Square 2  5))
                  , ((Square 4  7), (Square 4  6))]
            let t = Chess.positionTree p
            let kingMoves = filter (\p -> pieceAt p (Square 5  1) == Nothing) t
            length kingMoves `shouldBe` (3 :: Int)
        it "finds two en passant moves for white" $ do
            let p = Chess.makeMoves Chess.startPosition [ ((Square 5  2), (Square 5  4))
                  , ((Square 2  8), (Square 1  6))
                  , ((Square 5  4), (Square 5  5))
                  , ((Square 1  6), (Square 2  8))
                  , ((Square 3  2), (Square 3  4))
                  , ((Square 2  8), (Square 1  6))
                  , ((Square 3  4), (Square 3  5))
                  , ((Square 4  7), (Square 4  5))]
            let t = Chess.positionTree p
            let cPawnMoves = filter (\p -> pieceAt p (Square 3 5) == Nothing) t
            length cPawnMoves `shouldBe` (2 :: Int)
            let ePawnMoves = filter (\p -> pieceAt p (Square 5 5) == Nothing) t
            length ePawnMoves `shouldBe` (2 :: Int)
        it "counts occurrences of a position in a game history" $ do
            let p = Chess.makeMoves Chess.startPosition [ ((Square 2  1), (Square 3  3))
                 , ((Square 2  8), (Square 1  6))
                 , ((Square 3  3), (Square 2  1))
                 , ((Square 1  6), (Square 2  8))
                 , ((Square 2  1), (Square 3  3))
                 , ((Square 2  8), (Square 1  6))
                 , ((Square 3  3), (Square 2  1))
                 , ((Square 1  6), (Square 2  8))]
            Chess.threefoldrepetition p `shouldBe` True
        it "does not trigger 3-fold-repetition rule out of the blue" $ do
            let p = Chess.makeMoves Chess.startPosition [ ((Square 5  2), (Square 5  4))
                 , ((Square 1  7), (Square 1  5))
                 , ((Square 4  2), (Square 4  4))
                 , ((Square 1  5), (Square 1  4))
                 , ((Square 1  2), (Square 1  3))]
            Chess.threefoldrepetition p `shouldBe` False
        it "parses an en passant move for black" $ do
            let moves = ["e2-e4", "a7-a5", "f1-b5", "a5-a4", "b2-b4"]
            let p = parseMoves moves
            let p2 = Move.parseMove "a4-b3" p
            length (gamehistory p2) `shouldBe` (6 :: Int)
        it "allows white to take with pawns from home row" $ do
            let moves = ["e2-e4", "a7-a5", "e4-e5", "a5-a4", "e5-e6", "a4-a3"]
            let p = Move.parseMoves moves
            let p2 = Move.parseMove "b2-a3" p
            (length (gamehistory p2)) - (length (gamehistory p)) `shouldBe` (1 :: Int)
        it "does not allow black any crazy en passant moves on row 3" $ do
            let moves = ["e2-e4", "a7-a5", "e4-e5", "a5-a4", "e5-e6", "a4-a3", "b2-b3"]
            let p = Move.parseMoves moves
            let t = Chess.positionTree p
            let bPawnMoves = filter (\p -> pieceAt p (Square 1  3) == Nothing) t
            length (bPawnMoves) `shouldBe` (0 :: Int)
