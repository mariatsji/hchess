module MoveSpec where

import Chess
import Move
import Position

import Relude
import Test.Hspec

spec :: Spec
spec = do
    describe "board" $ do
        it "creates a board with 64 squares" $
            length board
                `shouldBe` (64 :: Int)
        it "moves E2-E4 from start pos" $ do
            let newPos = movePiece startPosition (Square 5 2) (Square 5 4)
            pieceAt newPos (Square 5 4) `shouldBe` (Just $ Pawn White :: Maybe Piece)
        it "finds 16 white pieces in startpos and all in the first 16 squares" $
            length (searchForPieces startPosition (\(Square _ r) -> r < 3) (\p -> colr p == White))
                `shouldBe` (16 :: Int)
        it "finds 16 black pieces in startpos and all in the last 16 squares" $
            length (searchForPieces startPosition (\(Square _ r) -> r > 6) (\p -> colr p == Black))
                `shouldBe` (16 :: Int)
    describe "Move" $ do
        it "finds 20 possible opening moves for white" $ do
            let tree = positionTree startPosition
            length tree `shouldBe` (20 :: Int)
        it "flips toPlay in all positions in a positionTree" $ do
            let tree = positionTree startPosition
                toPlayBlack = filter ((== Black) . toPlay) tree
            length toPlayBlack `shouldBe` length tree
        it "flips toPlay in all positions when mate" $ do
            let p1 = movePiece startPosition (Square 5 2) (Square 5 4)
                p2 = movePiece p1 (Square 5 7) (Square 5 5)
                p3 = movePiece p2 (Square 6 1) (Square 3 4)
                p4 = movePiece p3 (Square 1 7) (Square 1 6)
                p5 = movePiece p4 (Square 4 1) (Square 8 5)
                p6 = movePiece p5 (Square 2 8) (Square 3 6)
                p7 = movePiece p6 (Square 8 5) (Square 6 7) -- mate
            toPlay p7 `shouldBe` Black
        it "parses a move text command" $ do
            let newP = Move.playMove "e2-e4z" startPosition
            newP `shouldSatisfy` isRight
            newP `shouldNotBe` Right startPosition
        it "finds toSquares for pawns in startrow" $ do
            let squares = toSquaresPawn startPosition (Square 5 2)
            squares `shouldMatchList` ([(Square 5 3, Nothing), (Square 5 4, Nothing)] :: [(Square, Maybe Square)])
        it "finds a small number of end-positions" $ do
            let p1 = replacePieceAt (m emptyBoard) (Square 8 8) (King Black)
            let p2 = replacePieceAt p1 (Square 5 1) (King White)
            let p3 = replacePieceAt p2 (Square 8 7) (Pawn White)
            let t = positionTreeIgnoreCheck Position {m = p3, gamehistory = [m emptyBoard], pristineLongWhite = True, pristineShortWhite = True, pristineShortBlack = True, pristineLongBlack = True}
            length t `shouldBe` (3 :: Int)
        it "knows that white is in check" $ do
            let p' = Move.playMoves ["e2-e4", "d7-d5", "e4-d5", "d8-d5", "h2-h4", "d5-e5"]
            p' `shouldSatisfy` isRight
            let p = fromRight startPosition p'
            isInCheck (m p) White `shouldBe` (True :: Bool)
        it "knows that white is not in check" $ do
            let p' = Move.playMoves ["e2-e4", "d7-d5", "e4-d5", "d8-d5", "h2-h4", "d5-a5"]
            p' `shouldSatisfy` isRight
            let p = fromRight startPosition p'
            isInCheck (m p) White `shouldBe` (False :: Bool)
        it "does not change color on black pawns on the board when white promotes to a rook" $ do
            let Right p1 = playMoves ["e2-e4", "d7-d5", "e4-d5", "c7-c6", "d5-c6", "a7-a5", "c6-b7", "a5-a4", "b7-a8R"]
            pieceAt p1 (Square 1 4) `shouldBe` Just (Pawn Black)
            pieceAt p1 (Square 1 8) `shouldBe` Just (Rook White)
        it "allows castle both sides for white after opening move" $ do
            let Right p = Move.playMoves ["e2-e4", "d7-d5", "e4-d5", "d8-d5", "h2-h4", "d5-a5"]
            pristineShortWhite p `shouldBe` True
            pristineLongWhite p `shouldBe` True
            pristineShortBlack p `shouldBe` True
            pristineLongBlack p `shouldBe` True
        it "only allows white to castle long after moving rook on a1" $ do
            let Right p = Move.playMoves ["a2-a4", "d7-d5", "a1-a2"]
            pristineShortWhite p `shouldBe` True
            pristineLongWhite p `shouldBe` False
            pristineShortBlack p `shouldBe` True
            pristineLongBlack p `shouldBe` True
        it "sais pristine long black when long castle is still on the table" $ do
            let Right p = Move.playMoves ["e2-e4", "b8-c6", "d1-g4", "b7-b6", "b1-c3", "c8-b7", "g1-f3", "e7-e6", "g4-g5", "d8-f6", "f1-d3"]
            pristineLongBlack p `shouldBe` True
        it "includes long castles for black after intermittent pieces moved out in positionTree" $ do
            let Right p = Move.playMoves ["e2-e4", "b8-c6", "d1-g4", "b7-b6", "b1-c3", "c8-b7", "g1-f3", "e7-e6", "g4-g5", "d8-f6", "f1-d3"]
            length (possibleCastles p) `shouldBe` 1
        it "includes long castles for black after intermittent pieces moved out" $ do
            let Right p = Move.playMoves ["e2-e4", "b8-c6", "d1-g4", "b7-b6", "b1-c3", "c8-b7", "g1-f3", "e7-e6", "g4-g5", "d8-f6", "f1-d3"]
            length (positionTree p) `shouldBe` 42
        it "allows black to castle long after moving out intermittent pieces" $ do
            let Right p = Move.playMoves ["e2-e4", "b8-c6", "d1-g4", "b7-b6", "b1-c3", "c8-b7", "g1-f3", "e7-e6", "g4-g5", "d8-f6", "f1-d3", "O-O-O"]
            pristineShortWhite p `shouldBe` True
            pristineLongWhite p `shouldBe` True
            pristineShortBlack p `shouldBe` False
            pristineLongBlack p `shouldBe` False
        it "does not allow black any castle after moving king" $ do
            let Right p = Move.playMoves ["e2-e4", "e7-e5", "d2-d4", "e8-e7"]
            pristineShortWhite p `shouldBe` True
            pristineLongWhite p `shouldBe` True
            pristineShortBlack p `shouldBe` False
            pristineLongBlack p `shouldBe` False
        it "does a short castle for black" $ do
            let initMoves = ["e2-e4", "e7-e5", "g1-f3", "g8-f6", "f1-e2", "f8-e7", "O-O", "O-O"]
                Right p1 = playMoves initMoves
            pieceAt p1 (Square 5 8) `shouldBe` Nothing
            pieceAt p1 (Square 8 8) `shouldBe` Nothing
            pieceAt p1 (Square 7 8) `shouldBe` Just (King Black)
            pieceAt p1 (Square 6 8) `shouldBe` Just (Rook Black)
        it "does a long castle for black when the startpos is used" $ do
            let m1 = removePieceAt (m startPosition) (Square 2 8)
                m2 = removePieceAt m1 (Square 3 8)
                m3 = removePieceAt m2 (Square 4 8)
                p = Position m3 [m2, m1, m startPosition] True True True True
                Just c = castle CastleLong p
            pieceAt c (Square 3 8) `shouldBe` Just (King Black)
            pieceAt c (Square 4 8) `shouldBe` Just (Rook Black)
        it "includes long castle for white in legal moves" $ do
            let moves = ["d2-d4", "d7-d5", "b1-c3", "e7-e5", "b2-b3", "f7-f5", "c1-b2", "g7-g5", "d1-d2", "h7-h5"]
            let p' = Move.playMoves moves
            p' `shouldSatisfy` isRight
            let p = fromRight startPosition p'
            let legals = positionTree p
            let kingMoves = filter (\p -> isNothing (pieceAt p (Square 5 1))) legals
            length kingMoves `shouldBe` (2 :: Int)
        it "parses a long castle for white" $ do
            let moves = ["d2-d4", "d7-d5", "b1-c3", "e7-e5", "b2-b3", "f7-f5", "c1-b2", "g7-g5", "d1-d2", "h7-h5"]
                p' = Move.playMoves moves
            p' `shouldSatisfy` isRight
            let p = fromRight startPosition p'
                p2' = Move.playMove "O-O-O" p
            p2' `shouldSatisfy` isRight
            let p2 = fromRight startPosition p2'
            length (gamehistory p2) - length (gamehistory p) `shouldBe` (1 :: Int)
        it "white does not castle through check" $ do
            let p =
                    makeMoves
                        startPosition
                        [ (Square 5 2, Square 5 4)
                        , (Square 5 7, Square 5 5)
                        , (Square 7 1, Square 6 3)
                        , (Square 2 8, Square 3 6)
                        , (Square 6 1, Square 2 5)
                        , (Square 4 7, Square 4 6)
                        , (Square 2 1, Square 3 3)
                        , (Square 4 8, Square 7 5)
                        , (Square 3 3, Square 4 5)
                        , (Square 7 5, Square 7 2)
                        ]
            let p2 = castle CastleShort p
            p2 `shouldBe` Nothing
        it "lets white castle from moves out of the opening" $ do
            let p =
                    makeMoves
                        startPosition
                        [ (Square 5 2, Square 5 4)
                        , (Square 5 7, Square 5 5)
                        , (Square 7 1, Square 6 3)
                        , (Square 2 8, Square 3 6)
                        , (Square 6 1, Square 2 5)
                        , (Square 4 7, Square 4 6)
                        ]
            let t = positionTree p
            let kingMoves = filter (\p -> isNothing (pieceAt p (Square 5 1))) t
            length kingMoves `shouldBe` (3 :: Int)
        it "finds two en passant moves for white" $ do
            let p =
                    makeMoves
                        startPosition
                        [ (Square 5 2, Square 5 4)
                        , (Square 2 8, Square 1 6)
                        , (Square 5 4, Square 5 5)
                        , (Square 1 6, Square 2 8)
                        , (Square 3 2, Square 3 4)
                        , (Square 2 8, Square 1 6)
                        , (Square 3 4, Square 3 5)
                        , (Square 4 7, Square 4 5)
                        ]
            let t = positionTree p
            let cPawnMoves = filter (\p -> isNothing (pieceAt p (Square 3 5))) t
            length cPawnMoves `shouldBe` (2 :: Int)
            let ePawnMoves = filter (\p -> isNothing (pieceAt p (Square 5 5))) t
            length ePawnMoves `shouldBe` (2 :: Int)
        it "does not trigger 3-fold-repetition rule out of the blue" $ do
            let p =
                    makeMoves
                        startPosition
                        [ (Square 5 2, Square 5 4)
                        , (Square 1 7, Square 1 5)
                        , (Square 4 2, Square 4 4)
                        , (Square 1 5, Square 1 4)
                        , (Square 1 2, Square 1 3)
                        ]
            threefoldrepetition p `shouldBe` False
        it "finds threefoldrepetition though" $ do
            let moves = ["b1-c3", "b8-c6", "c3-b1", "c6-b8", "b1-c3", "b8-c6", "c3-b1", "c6-b8", "b1-c3", "b8-c6", "c3-b1", "c6-b8"]
                Right p = playMoves moves
            threefoldrepetition p `shouldBe` True
        it "parses an en passant move for black" $ do
            let moves = ["e2-e4", "a7-a5", "f1-b5", "a5-a4", "b2-b4"]
            let p' = playMoves moves
            p' `shouldSatisfy` isRight
            let p = fromRight startPosition p'
            let p2' = Move.playMove "a4-b3" p
            p2' `shouldSatisfy` isRight
            let p2 = fromRight startPosition p2'
            length (gamehistory p2) `shouldBe` (6 :: Int)
        it "allows white to take with pawns from home row" $ do
            let moves = ["e2-e4", "a7-a5", "e4-e5", "a5-a4", "e5-e6", "a4-a3", "b2-a3"]
            let p = Move.playMoves moves
            p `shouldSatisfy` isRight
        it "does not allow black any crazy en passant moves on row 3" $ do
            let moves = ["e2-e4", "a7-a5", "e4-e5", "a5-a4", "e5-e6", "a4-a3", "b2-b3"]
            let p = Move.playMoves moves
            let t = positionTree (fromRight startPosition p)
            let bPawnMoves = filter (\p -> isNothing (pieceAt p (Square 1 3))) t
            length bPawnMoves `shouldBe` (0 :: Int)
        it "records gamehistory correctly" $ do
            let p1 = startPosition
            let (p2:_) = positionTree startPosition
            gamehistory p2 `shouldBe` [m p1]
        it "knows black is in check" $ do
            let Right p = playMoves ["e2-e4", "e7-e5", "f1-c4", "b8-c6", "d1-h5", "g8-f6", "h5-f7"]
            isInCheck (m p) Black `shouldBe` True
        it "has empty positionTree when black is checkmate" $ do
            let Right p = playMoves ["e2-e4", "e7-e5", "f1-c4", "b8-c6", "d1-h5", "g8-f6", "h5-f7"]
                ptree = positionTree p
            null ptree `shouldBe` True
        it "realizes black is checkmate" $ do
            let Right p = playMoves ["e2-e4", "e7-e5", "f1-c4", "b8-c6", "d1-h5", "g8-f6", "h5-f7"]
                ptree = positionTree p
            isCheckMate p ptree `shouldBe` True
        it "determines status of a checkmate" $ do
            let Right p = playMoves ["e2-e4", "e7-e5", "f1-c4", "b8-c6", "d1-h5", "g8-f6", "h5-f7"]
            determineStatus p (positionTree p) `shouldBe` BlackIsMate
        it "knows what move has been played between two snapshots" $ do
            let Right p1 = playMoves ["e2-e4", "e7-e5"]
                Right p2 = playMoves ["e2-e4", "e7-e5", "f1-c4"]
                themove = findMove (m p1) (m p2)
            themove `shouldBe` MovedPiece (Square 6 1) (Square 3 4)
        it "knows black castle short between two snapshots" $ do
            let initMoves = ["e2-e4", "e7-e5", "g1-f3", "g8-f6", "f1-e2", "f8-e7"]
                Right p1 = playMoves initMoves
                Right p2 = playMoves (initMoves <> ["O-O"])
                Right p3 = playMoves (initMoves <> ["O-O", "O-O"])
                whiteCastle = findMove (m p1) (m p2)
                blackCastle = findMove (m p2) (m p3)
            whiteCastle `shouldBe` CastleShort
            blackCastle `shouldBe` CastleShort
        it "knows a promotion move for white between two snapshots" $ do
            let Right p1 = playMoves ["e2-e4", "d7-d5", "e4-d5", "c7-c6", "d5-c6", "a7-a5", "c6-b7", "a5-a4"]
                Right p2 = playMove "b7-a8R" p1
                themove = findMove (m p1) (m p2)
            themove `shouldBe` Promotion (Square 2 7) (Square 1 8) (Rook White)
        it "can move a black king and find position between snapshots" $ do
            let Right p1 = playMoves ["e2-e4", "d7-d5", "e4-e5"]
                Right p2 = playMove "e8-d7" p1
                themove = findMove (m p1) (m p2)
            themove `shouldBe` MovedPiece (Square 5 8) (Square 4 7)
        it "finds epfromsquare and eptosquare" $ do
            epfromSquare [(Square 5 5, Nothing), (Square 6 5, Nothing), (Square 6 6, Just (Pawn White))] `shouldBe` Square 5 5
            eptoSquare [(Square 5 5, Nothing), (Square 6 5, Nothing), (Square 6 6, Just (Pawn White))] `shouldBe` Square 6 6
        it "finds promotions in positionTree" $ do
            let Right p1 = playMoves ["e2-e4", "d7-d5", "e4-d5", "c7-c6", "d5-c6", "a7-a5", "c6-b7", "a5-a4"]
                Right p2 = playMove "b7-a8R" p1
                Right p3 = playMove "b7-a8Q" p1
                Right p4 = playMove "b7-a8B" p1
                Right p5 = playMove "b7-a8K" p1
            p2 `elem` positionTree p1 `shouldBe` True
            p3 `elem` positionTree p1 `shouldBe` True
            p4 `elem` positionTree p1 `shouldBe` True
            p5 `elem` positionTree p1 `shouldBe` True
        it "finds a walk in promotion, not just a take in" $ do
            let Right p1 = playMoves ["e2-e4", "d7-d5", "e4-d5", "c7-c6", "d5-c6", "a7-a5", "c6-b7", "b8-a6"]
                Right p2 = playMove "b7-b8K" p1
                themove = findMove (m p1) (m p2)
            pieceAt p2 (Square 2 8) `shouldBe` Just (Knight White)
            themove `shouldBe` Promotion (Square 2 7) (Square 2 8) (Knight White)
        it "finds enPassant" $ do
            let Right p = playMoves ["b2-b4", "h7-h5", "b4-b5", "a7-a5"]
            enPassant p (Square 1 5) `shouldBe` True
        it "finds enPassant in move form as well" $ do
            let Right p = playMoves ["b2-b4", "h7-h5", "b4-b5", "a7-a5", "b5-a6"]
                (lastMove : _) = gamehistory p
            findMove lastMove (m p) `shouldBe` EnPassant (Square 2 5) (Square 1 6)