module MoveSpec where

import Bunch
import Chess
import Control.Exception (evaluate)
import Data.Either
import qualified Data.Map.Lazy as Map
import Data.Maybe (isNothing)
import Move
import Position
import Printer
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "board" $ do
    it "prints the start position"
      $ Printer.pretty startPosition
    it "creates a board with 64 squares"
      $ length board
      `shouldBe` (64 :: Int)
    it "moves E2-E4 from start pos" $ do
      let newPos = movePiece startPosition (Square 5 2) (Square 5 4)
      pieceAt newPos (Square 5 4) `shouldBe` (Just $ Pawn White :: Maybe Piece)
    it "finds 16 white pieces in startpos and all in the first 16 squares"
      $ length (searchForPieces startPosition (\(Square _ r) -> r < 3) (\p -> colr p == White))
      `shouldBe` (16 :: Int)
    it "finds 16 black pieces in startpos and all in the last 16 squares"
      $ length (searchForPieces startPosition (\(Square _ r) -> r > 6) (\p -> colr p == Black))
      `shouldBe` (16 :: Int)
  describe "Move" $ do
    it "finds 20 possible opening moves for white" $ do
      let tree = positionTree startPosition
      length tree `shouldBe` (20 :: Int)
    it "parses a move text command" $ do
      let newP = Move.parseMove "e2-e4" startPosition
      newP `shouldSatisfy` isRight
      newP `shouldNotBe` Right startPosition
    it "does not step on own pieces" $ do
      let b = canGoThere startPosition (Square 1 1) (Square 1 2)
      b `shouldBe` (False :: Bool)
    it "lets pawns move ahead from startpos" $ do
      let b = canGoThere startPosition (Square 5 2) (Square 5 4)
      b `shouldBe` (True :: Bool)
    it "knows when destination square is occupied by own color" $ do
      let b = finalDestinationNotOccupiedBySelf startPosition (Square 1 1) (Square 1 2)
      b `shouldBe` (False :: Bool)
    it "finds the correct traversed squares in a straight bishop-like move" $ do
      let squares = points (Square 5 3) (Square 7 5)
      squares `shouldBe` ([Square 6 4] :: [Square])
    it "finds the correct traversed squares in a straight rook-like move" $ do
      let squares = points (Square 1 1) (Square 1 4)
      squares `shouldBe` ([Square 1 2, Square 1 3] :: [Square])
    it "finds the correct traversed squares from h1 - a8" $ do
      let squares = points (Square 8 1) (Square 1 8)
      squares `shouldBe` ([Square 7 2, Square 6 3, Square 5 4, Square 4 5, Square 3 6, Square 2 7] :: [Square])
    it "finds toSquares for pawns in startrow" $ do
      let squares = toSquaresPawn startPosition (Square 5 2, Pawn White)
      squares `shouldMatchList` ([(Square 5 3, Nothing), (Square 5 4, Nothing)] :: [(Square, Maybe Square)])
    it "finds a small number of end-positions" $ do
      let p1 = replacePieceAt (m emptyBoard) (Square 8 8) (King Black)
      let p2 = replacePieceAt p1 (Square 5 1) (King White)
      let p3 = replacePieceAt p2 (Square 8 7) (Pawn White)
      let t = positionTreeIgnoreCheck Position { m = p3, gamehistory = [m emptyBoard], castleStatusWhite = CanCastleBoth, castleStatusBlack = CanCastleBoth, whiteKing = Just (Square 5 1), blackKing = Just (Square 8 8) }
      length t `shouldBe` (3 :: Int)
    it "knows that white is in check" $ do
      let p' = Move.parseMoves ["e2-e4", "d7-d5", "e4-d5", "d8-d5", "h2-h4", "d5-e5"]
      p' `shouldSatisfy` isRight
      let p = fromRight startPosition p'
      isInCheck p (toPlay p) `shouldBe` (True :: Bool)
    it "knows that white is not in check" $ do
      let p' = Move.parseMoves ["e2-e4", "d7-d5", "e4-d5", "d8-d5", "h2-h4", "d5-a5"]
      p' `shouldSatisfy` isRight
      let p = fromRight startPosition p'
      isInCheck p (toPlay p) `shouldBe` (False :: Bool)
    it "promotes pawns for Black " $ do
      let m1 = replacePieceAt (m emptyBoard) (Square 8 1) (Pawn Black)
      let p2 = promote Black (Position m1 [m startPosition] CanCastleBoth CanCastleBoth (Just $ Square 5 1) (Just $ Square 5 8))
      pieceAt (head p2) (Square 8 1) `shouldBe` Just (Queen Black)
      pieceAt (head $ tail p2) (Square 8 1) `shouldBe` Just (Rook Black)
      pieceAt (head $ tail $ tail p2) (Square 8 1) `shouldBe` Just (Bishop Black)
      pieceAt (last p2) (Square 8 1) `shouldBe` Just (Knight Black)
    it "finds promotion positions for White" $ do
      let m1 = replacePieceAt (m emptyBoard) (Square 5 8) (Pawn White)
      let t = promoteBindFriendly White (Position m1 [] CanCastleBoth CanCastleBoth (Just $ Square 5 1) (Just $ Square 5 8))
      length t `shouldBe` (4 :: Int)
    it "leaves unpromotable boards alone for White" $ do
      let m1 = replacePieceAt (m emptyBoard) (Square 5 7) (Pawn White)
      let t = promoteBindFriendly White (Position m1 [] CanCastleBoth CanCastleBoth (Just $ Square 5 1) (Just $ Square 5 8))
      t `shouldBe` Bunch [Position m1 [] CanCastleBoth CanCastleBoth (Just $ Square 5 1) (Just $ Square 5 8)]
    it "promotes passed pawns for Black in the position tree" $ do
      let m1 = replacePieceAt (m emptyBoard) (Square 5 2) (Pawn Black)
          p1 = Position m1 [m startPosition] CanCastleBoth CanCastleBoth (Just $ Square 5 1) (Just $ Square 5 8)
      let t = positionTree p1
      pieceAt (unsafeHead t) (Square 5 1) `shouldBe` Just (Queen Black)
      length t `shouldBe` (4 :: Int)
    it "allows castle both sides for white after opening move" $ do
      let p = Move.parseMoves ["e2-e4", "d7-d5", "e4-d5", "d8-d5", "h2-h4", "d5-a5"]
      either (const CanCastleNone) castleStatusWhite p `shouldBe` CanCastleBoth
      either (const CanCastleNone) castleStatusBlack p `shouldBe` CanCastleBoth
    it "only allows white to castle kingside after moving rook on a1" $ do
      let p = Move.parseMoves ["a2-a4", "d7-d5", "a1-a2"]
      either (const CanCastleBoth) castleStatusWhite p `shouldBe` CanCastleH
      either (const CanCastleNone) castleStatusBlack p `shouldBe` CanCastleBoth
    it "does not allow black any castle after moving king" $ do
      let p = Move.parseMoves ["e2-e4", "e7-e5", "d2-d4", "e8-e7"]
      either (const CanCastleNone) castleStatusWhite p `shouldBe` CanCastleBoth
      either (const CanCastleBoth) castleStatusBlack p `shouldBe` CanCastleNone
    it "does a long castle for black when the startpos is used" $ do
      let m1 = removePieceAt (m startPosition) (Square 2 8)
          m2 = removePieceAt m1 (Square 3 8)
          m3 = removePieceAt m2 (Square 4 8)
          p = Position m3 [m2, m1, m startPosition] CanCastleBoth CanCastleBoth (Just $ Square 5 1) (Just $ Square 5 8)
          c = castleLong p Black
      length c `shouldBe` (1 :: Int)
      pieceAt (head c) (Square 3 8) `shouldBe` Just (King Black)
      pieceAt (head c) (Square 4 8) `shouldBe` Just (Rook Black)
    it "includes long castle for white in legal moves" $ do
      let moves = ["d2-d4", "d7-d5", "b1-c3", "e7-e5", "b2-b3", "f7-f5", "c1-b2", "g7-g5", "d1-d2", "h7-h5"]
      let p' = Move.parseMoves moves
      p' `shouldSatisfy` isRight
      let p = fromRight startPosition p'
      let legals = positionTree p
      let kingMoves = filter' (\p -> isNothing (pieceAt p (Square 5 1))) legals
      length kingMoves `shouldBe` (2 :: Int)
    it "parses a long castle for white" $ do
      let moves = ["d2-d4", "d7-d5", "b1-c3", "e7-e5", "b2-b3", "f7-f5", "c1-b2", "g7-g5", "d1-d2", "h7-h5"]
          p' = Move.parseMoves moves
      p' `shouldSatisfy` isRight
      let p = fromRight startPosition p'
          p2' = Move.parseMove "O-O-O" p
      p2' `shouldSatisfy` isRight
      let p2 = fromRight startPosition p2'
      length (gamehistory p2) - length (gamehistory p) `shouldBe` (1 :: Int)
    it "white does not castle through check" $ do
      let p =
            makeMoves startPosition
              [ (Square 5 2, Square 5 4),
                (Square 5 7, Square 5 5),
                (Square 7 1, Square 6 3),
                (Square 2 8, Square 3 6),
                (Square 6 1, Square 2 5),
                (Square 4 7, Square 4 6),
                (Square 2 1, Square 3 3),
                (Square 4 8, Square 7 5),
                (Square 3 3, Square 4 5),
                (Square 7 5, Square 7 2)
                ]
      let p2 = castleShort p White
      p2 `shouldBe` []
    it "lets white castle from moves out of the opening" $ do
      let p =
            makeMoves startPosition
              [ (Square 5 2, Square 5 4),
                (Square 5 7, Square 5 5),
                (Square 7 1, Square 6 3),
                (Square 2 8, Square 3 6),
                (Square 6 1, Square 2 5),
                (Square 4 7, Square 4 6)
                ]
      let t = positionTree p
      let kingMoves = filter' (\p -> isNothing (pieceAt p (Square 5 1))) t
      length kingMoves `shouldBe` (3 :: Int)
    it "finds two en passant moves for white" $ do
      let p =
            makeMoves startPosition
              [ (Square 5 2, Square 5 4),
                (Square 2 8, Square 1 6),
                (Square 5 4, Square 5 5),
                (Square 1 6, Square 2 8),
                (Square 3 2, Square 3 4),
                (Square 2 8, Square 1 6),
                (Square 3 4, Square 3 5),
                (Square 4 7, Square 4 5)
                ]
      let t = positionTree p
      let cPawnMoves = filter' (\p -> isNothing (pieceAt p (Square 3 5))) t
      length cPawnMoves `shouldBe` (2 :: Int)
      let ePawnMoves = filter' (\p -> isNothing (pieceAt p (Square 5 5))) t
      length ePawnMoves `shouldBe` (2 :: Int)
    it "counts occurrences of a position in a game history" $ do
      let p =
            makeMoves startPosition
              [ (Square 2 1, Square 3 3),
                (Square 2 8, Square 1 6),
                (Square 3 3, Square 2 1),
                (Square 1 6, Square 2 8),
                (Square 2 1, Square 3 3),
                (Square 2 8, Square 1 6),
                (Square 3 3, Square 2 1),
                (Square 1 6, Square 2 8)
                ]
      threefoldrepetition p `shouldBe` True
    it "does not trigger 3-fold-repetition rule out of the blue" $ do
      let p =
            makeMoves startPosition
              [ (Square 5 2, Square 5 4),
                (Square 1 7, Square 1 5),
                (Square 4 2, Square 4 4),
                (Square 1 5, Square 1 4),
                (Square 1 2, Square 1 3)
                ]
      threefoldrepetition p `shouldBe` False
    it "finds threefoldrepetition though" $ do
      let moves = ["b1-c3", "b8-c6", "c3-b1", "c6-b8", "b1-c3", "b8-c6", "c3-b1", "c6-b8", "b1-c3", "b8-c6", "c3-b1", "c6-b8"]
          Right p = parseMoves moves
      threefoldrepetition p `shouldBe` True
    it "parses an en passant move for black" $ do
      let moves = ["e2-e4", "a7-a5", "f1-b5", "a5-a4", "b2-b4"]
      let p' = parseMoves moves
      p' `shouldSatisfy` isRight
      let p = fromRight startPosition p'
      let p2' = Move.parseMove "a4-b3" p
      p2' `shouldSatisfy` isRight
      let p2 = fromRight startPosition p2'
      length (gamehistory p2) `shouldBe` (6 :: Int)
    it "allows white to take with pawns from home row" $ do
      let moves = ["e2-e4", "a7-a5", "e4-e5", "a5-a4", "e5-e6", "a4-a3", "b2-a3"]
      let p = Move.parseMoves moves
      p `shouldSatisfy` isRight
    it "does not allow black any crazy en passant moves on row 3" $ do
      let moves = ["e2-e4", "a7-a5", "e4-e5", "a5-a4", "e5-e6", "a4-a3", "b2-b3"]
      let p = Move.parseMoves moves
      let t = positionTree (fromRight startPosition p)
      let bPawnMoves = filter' (\p -> isNothing (pieceAt p (Square 1 3))) t
      length bPawnMoves `shouldBe` (0 :: Int)
    it "records gamehistory correctly" $ do
      let p1 = startPosition
      let p2 = unsafeHead $ positionTree startPosition
      head (gamehistory p2) `shouldBe` m p1
    it "finds an empty position tree for black when black is checkmate" $ do
      let Right p = parseMoves ["e2-e4", "e7-e5", "f1-c4","b8-c6","d1-h5","g8-f6","h5-f7"]
      let p' = head $ unBunch (positionTree p)
      length p' `shouldBe` 0
    it "realizes black is checkmate" $ do
      let Right p = parseMoves ["e2-e4", "e7-e5", "f1-c4","b8-c6","d1-h5","g8-f6","h5-f7"]
      isCheckMate p `shouldBe` True
    it "determines status of a checkmate" $ do
      let Right p = parseMoves ["e2-e4", "e7-e5", "f1-c4","b8-c6","d1-h5","g8-f6","h5-f7"]
      determineStatus p `shouldBe` BlackIsMate
