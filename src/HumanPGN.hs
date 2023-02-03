module HumanPGN (parsePgn) where

import Chess (movePiece, pieceAt, playIfLegal, positionTree)
import Control.Applicative (many, (<|>))
import Control.Monad (foldM)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AT
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Debug.Trace as Debug
import GHC.Base (MonadPlus)
import Move (colParser, rowParser, squareParser)
import NeatInterpolation
import Position (Color (..), Move (CastleLong, CastleShort, MovedPiece, Promotion), Piece (..), Position (..), Square (..), searchForPieces, startPosition)

roll :: MonadPlus m => a -> [a -> m a] -> m a -- fishy, the function is the list is the same every time, so not necessary to put it i a list
roll =
    foldM
        ( \a f -> f a
        )

parsePgn :: Text -> Either String Position
parsePgn =
    AT.parseOnly
        ( do
            _ <- many metaLine
            _ <- AT.skipWhile AT.isEndOfLine
            movesParser
        )

data Meta = Meta Text Text
    deriving stock (Show)

metaLine :: Parser Meta
metaLine = do
    _ <- AT.char '['
    key <- AT.takeTill AT.isHorizontalSpace
    _ <- AT.space
    val <- AT.takeTill (== ']')
    _ <- AT.char ']'
    _ <- AT.endOfLine
    pure $ Meta key val

movesParser' :: Parser Position
movesParser' = do
    p1 <- whiteBlackParser startPosition
    p2 <- whiteBlackParser p1
    pure p2

movesParser :: Parser Position
movesParser = do
    let list = replicate 3 whiteBlackParser
    roll startPosition list

pgnMoveParser :: Position -> Parser Position
pgnMoveParser pos =
    let c = toPlay pos
        castleLongParser = do
            _ <- AT.string "O-O-O"
            case playIfLegal CastleLong pos of
                Right newPos -> pure newPos
                Left s -> fail s
        castleShortParser = do
            _ <- AT.string "O-O"
            case playIfLegal CastleShort pos of
                Right newPos -> pure newPos
                Left s -> fail s
        promParser = do
            -- c7-c8=Q
            _ <- AT.skipWhile (`elem` ['N', 'B', 'R', 'Q', 'K'])
            fromS <- squareParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.char '='
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            _ <- AT.skipWhile (`elem` ['!', '#'])
            let move = Promotion fromS toS piece
            case playIfLegal move pos of
                Right newPos -> pure newPos
                Left s -> fail s
        shortPawnTakesPromParser = do
            -- bxc8=K
            fromCol <- colParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.char '='
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            _ <- AT.skipWhile (`elem` ['!', '#'])
            let fromS = before toS {col = fromCol} c
            if pieceAt pos fromS == Just (Pawn c)
                then
                    let move = Promotion fromS toS piece
                     in case playIfLegal move pos of
                            Right newPos -> pure newPos
                            Left s -> fail s
                else fail $ "No short promotion when unexpected pawn at " <> show fromS
        shortPromParser = do
            -- e8=Q
            toS <- squareParser
            _ <- AT.char '='
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            _ <- AT.skipWhile (`elem` ['!', '#'])
            let fromS = before toS c
            if pieceAt pos fromS == Just (Pawn c)
                then
                    let move = Promotion fromS toS piece
                     in case playIfLegal move pos of
                            Right newPos -> pure newPos
                            Left s -> fail s
                else fail $ "No short promotion when unexpected pawn at " <> show fromS
        regularMoveParser = do
            -- e1-e2
            _ <- AT.skipWhile (`elem` ['N', 'B', 'R', 'Q', 'K'])
            fromS <- squareParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.skipWhile (`elem` ['!', '#'])
            case playIfLegal (MovedPiece fromS toS) pos of
                Right newPos -> pure newPos
                Left s -> fail s
        shortOfficerColMove = do
            -- Kdc4 / Kdxc4
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            fromCol <- colParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.skipWhile (`elem` ['!', '#'])
            case findPiece pos piece (\(Square col' _) -> col' == fromCol) toS of
                Right fromS -> case playIfLegal (MovedPiece fromS toS) pos of
                    Right newPos -> pure newPos
                    Left s -> fail s
                Left s -> fail s
        shortOfficerRowMove = do
            -- K5c5 / K5xc5
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            fromRow <- rowParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.skipWhile (`elem` ['!', '#'])
            case findPiece pos piece (\(Square _ row') -> row' == fromRow) toS of
                Right fromS -> case playIfLegal (MovedPiece fromS toS) pos of
                    Right newPos -> pure newPos
                    Left s -> fail s
                Left s -> fail s
        shortPawnTakes = do
            -- bxc4
            fromCol <- colParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.skipWhile (`elem` ['!', '#'])
            case findPiece pos (Pawn c) (\(Square col' _) -> col' == fromCol) toS of
                Right fromS -> case playIfLegal (MovedPiece fromS toS) pos of
                    Right newPos -> pure newPos
                    Left s -> fail s
                Left s -> fail s
        shortOfficerMove = do
            -- Kc4 / Kxc4
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.skipWhile (`elem` ['!', '#'])
            case findPiece pos piece (const True) toS of
                Right fromS -> case playIfLegal (MovedPiece fromS toS) pos of
                    Right newPos -> pure newPos
                    Left s -> fail s
                Left s -> fail s
        shortPawnMove = do
            -- e4
            toS@(Square toCol _) <- squareParser
            _ <- AT.skipWhile (`elem` ['!', '#'])
            case findPiece pos (Pawn c) (\(Square col' _) -> col' == toCol) toS of
                Right fromS -> case playIfLegal (MovedPiece fromS toS) pos of
                    Right newPos -> pure newPos
                    Left s -> fail s
                Left s -> fail s
     in castleLongParser
            <|> castleShortParser
            <|> promParser
            <|> shortPawnTakesPromParser
            <|> shortPromParser
            <|> regularMoveParser
            <|> shortOfficerColMove
            <|> shortOfficerRowMove
            <|> shortPawnTakes
            <|> shortOfficerMove
            <|> shortPawnMove

findPiece :: Position -> Piece -> (Square -> Bool) -> Square -> Either String Square
findPiece pos piece squarePred toS = case searchForPieces pos squarePred piecePred of
    [(s, _)] -> Right s
    cands -> case filter (legal pos toS) cands of
        [(s, _)] -> Right s
        _ -> Left $ "No satisfactory " <> show piece <> " found"
  where
    piecePred p = p == piece

legal :: Position -> Square -> (Square, Piece) -> Bool
legal pos toS (fromS, _) =
    m (movePiece pos fromS toS) `elem` (m <$> positionTree pos)

before :: Square -> Color -> Square
before (Square c r) White = Square (c - 1) r
before (Square c r) Black = Square (c + 1) r

whiteBlackParser :: Position -> Parser Position
whiteBlackParser pos = do
    _ <- intParser
    _ <- AT.skipWhile (== '.')
    _ <- AT.space
    withReplyTerminated <|> withoutReplyTerminated <|> withReply <|> withoutReply
  where
    withReplyTerminated = do
        whiteMoved <- pgnMoveParser pos
        _ <- AT.space
        blackMoved <- pgnMoveParser whiteMoved
        _ <- AT.space
        _ <- result
        pure blackMoved
    withoutReplyTerminated = do
        whiteMoved <- pgnMoveParser pos
        _ <- AT.space
        _ <- result
        pure whiteMoved
    withReply = do
        whiteMoved <- pgnMoveParser pos
        _ <- AT.space
        blackMoved <- pgnMoveParser whiteMoved
        _ <- AT.space
        pure blackMoved
    withoutReply = do
        whiteMoved <- pgnMoveParser pos
        _ <- AT.space
        pure whiteMoved

result :: Parser Text
result = AT.string "*" <|> AT.string "1/2-1/2" <|> AT.string "1-0" <|> AT.string "0-1"

intParser :: Parser Int
intParser = fromMaybe 0 . toBoundedInteger <$> AT.scientific

testPgn :: Text
testPgn =
    [text|
[Event "Paris"]
[Site "Paris FRA"]
[Date "1858.??.??"]
[Round "?"]
[White "Paul Morphy"]
[Black "Duke Karl / Count Isouard"]
[Result "1-0"]

1. e4 e5 2. Nf3 *
|]

testPgn' :: Text
testPgn' =
    [text|
[Event "Paris"]
[Site "Paris FRA"]
[Date "1858.??.??"]
[EventDate "?"]
[Round "?"]
[Result "1-0"]
[White "Paul Morphy"]
[Black "Duke Karl / Count Isouard"]
[ECO "C41"]
[WhiteElo "?"]
[BlackElo "?"]
[PlyCount "33"]

1.e4 e5 2.Nf3 d6 3.d4 Bg4 4.dxe5 Bxf3 5.Qxf3 dxe5 6.Bc4 Nf6 7.Qb3 Qe7 8.Nc3 c6 9.Bg5  b5 10.Nxb5 cxb5 11.Bxb5+ Nbd7 12.O-O-O Rd8 13.Rxd7 Rxd7 14.Rd1 Qe6 15.Bxd7+ Nxd7 16.Qb8+ Nxb8 17.Rd8# 1-0
|]