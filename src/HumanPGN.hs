module HumanPGN (parsePgn) where

import Chess (movePiece, pieceAt, playIfLegal, positionTree)
import Control.Applicative (many, (<|>))
import Control.Monad (void)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AT
import Data.Text (Text)
import Move (colParser, rowParser, squareParser)
import NeatInterpolation
import Position (Color (..), Move (CastleLong, CastleShort, MovedPiece, Promotion), Piece (..), Position (..), Square (..), searchForPieces, startPosition)

roll :: a -> (a -> Parser a) -> Parser a
roll seed p = do
    new <- p seed
    (seed <$ AT.endOfInput) <|> roll new p

parsePgn :: Text -> Either String Position
parsePgn =
    AT.parseOnly
        ( do
            _ <- many metaLine
            _ <- AT.skipWhile AT.isEndOfLine
            _ <- AT.skipSpace
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

movesParser :: Parser Position
movesParser =
    roll startPosition whiteBlackParser

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
            _ <- AT.char '-'
            toS <- squareParser
            _ <- AT.char '='
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
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
            _ <- AT.char '-'
            toS <- squareParser
            case playIfLegal (MovedPiece fromS toS) pos of
                Right newPos -> pure newPos
                Left s -> fail s
        shortOfficerColMove = do
            -- Kdc4 / Kdxc4
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            fromCol <- colParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
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
            case findPiece pos piece (\(Square _ row') -> row' == fromRow) toS of
                Right fromS -> case playIfLegal (MovedPiece fromS toS) pos of
                    Right newPos -> pure newPos
                    Left s -> fail s
                Left s -> fail s
        shortOfficerMove = do
            -- Kc4 / Kxc4
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            case findPiece pos piece (const True) toS of
                Right fromS -> case playIfLegal (MovedPiece fromS toS) pos of
                    Right newPos -> pure newPos
                    Left s -> fail s
                Left s -> fail s
        shortPawnTakes = do
            -- bxc4
            fromCol <- colParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            case findPiece pos (Pawn c) (\(Square col' _) -> col' == fromCol) toS of
                Right fromS -> case playIfLegal (MovedPiece fromS toS) pos of
                    Right newPos -> pure newPos
                    Left s -> fail s
                Left s -> fail s
        shortPawnMove = do
            -- e4
            toS@(Square toCol _) <- squareParser
            case findPiece pos (Pawn c) (\(Square col' _) -> col' == toCol) toS of
                Right fromS -> case playIfLegal (MovedPiece fromS toS) pos of
                    Right newPos -> pure newPos
                    Left s -> fail s
                Left s -> fail s
     in ( castleLongParser
            <|> castleShortParser
            <|> promParser
            <|> shortPawnTakesPromParser
            <|> shortPromParser
            <|> regularMoveParser
            <|> shortOfficerColMove
            <|> shortOfficerRowMove
            <|> shortOfficerMove
            <|> shortPawnTakes
            <|> shortPawnMove
        )
            <* checkParser

checkParser :: Parser ()
checkParser = AT.skipWhile (`elem` ['+', '!', '#'])

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
    _ <- AT.skipWhile (\c -> c == '.' || c == ' ')
    withReplyTerminated <|> withoutReplyTerminated <|> withReply <|> withoutReply
  where
    withReplyTerminated = do
        whiteMoved <- pgnMoveParser pos
        _ <- sep
        blackMoved <- pgnMoveParser whiteMoved
        _ <- sep
        _ <- result
        pure blackMoved
    withoutReplyTerminated = do
        whiteMoved <- pgnMoveParser pos
        _ <- sep
        _ <- result
        pure whiteMoved
    withReply = do
        whiteMoved <- pgnMoveParser pos
        _ <- sep
        blackMoved <- pgnMoveParser whiteMoved
        _ <- sep
        pure blackMoved
    withoutReply = do
        whiteMoved <- pgnMoveParser pos
        _ <- sep
        pure whiteMoved

result :: Parser Text
result = AT.string "*" <|> AT.string "1/2-1/2" <|> AT.string "1-0" <|> AT.string "0-1"

intParser :: Parser Int
intParser = AT.decimal

sep :: Parser ()
sep = do
    void $ AT.many1 (void AT.space <|> AT.endOfLine <|> comment)

comment :: Parser ()
comment = AT.try . void $ AT.char '{' *> AT.manyTill' AT.anyChar (AT.char '}')

testPgn :: Text
testPgn =
    [text|
[Event "Casual game"]
[Site "London ENG"]
[Date "1858.08.??"]
[EventDate "?"]
[Round "?"]
[Result "0-1"]
[White "Henry Edward Bird"]
[Black "Paul Morphy"]
[ECO "C41"]
[WhiteElo "?"]
[BlackElo "?"]
[Source "The Field, 1858.09.04, p.192"]
[PlyCount "58"]

1.e4 e5 2.Nf3 d6 3.d4 f5 4.Nc3 fxe4 5.Nxe4 d5 6.Ng3 e4 7.Ne5
Nf6 8.Bg5 Bd6 9.Nh5 O-O 10.Qd2 Qe8 11.g4 Nxg4 12.Nxg4 Qxh5
13.Ne5 Nc6 14.Be2 Qh3 15.Nxc6 bxc6 16.Be3 Rb8 17.O-O-O Rxf2
18.Bxf2 Qa3 19.c3 Qxa2 20.b4 Qa1+ 21.Kc2 Qa4+ 22.Kb2 Bxb4
23.cxb4 Rxb4+ 24.Qxb4 Qxb4+ 25.Kc2 e3 26.Bxe3 Bf5+ 27.Rd3 Qc4+
28.Kd2 Qa2+ 29.Kd1 Qb1+ 0-1
|]