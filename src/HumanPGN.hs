module HumanPGN (parsePgn) where

import Chess (movePiece, pieceAt, playIfLegal, positionTree)
import Control.Applicative (many, (<|>))
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
            _ <- checkParser
            case playIfLegal CastleLong pos of
                Right newPos -> pure newPos
                Left s -> fail s
        castleShortParser = do
            _ <- AT.string "O-O"
            _ <- checkParser
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
            _ <- checkParser
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
            _ <- checkParser
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
            _ <- checkParser
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
            _ <- checkParser
            case playIfLegal (MovedPiece fromS toS) pos of
                Right newPos -> pure newPos
                Left s -> fail s
        shortOfficerColMove = do
            -- Kdc4 / Kdxc4
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            fromCol <- colParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- checkParser
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
            _ <- checkParser
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
            _ <- checkParser
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
            _ <- checkParser
            case findPiece pos (Pawn c) (\(Square col' _) -> col' == fromCol) toS of
                Right fromS -> case playIfLegal (MovedPiece fromS toS) pos of
                    Right newPos -> pure newPos
                    Left s -> fail s
                Left s -> fail s
        shortPawnMove = do
            -- e4
            toS@(Square toCol _) <- squareParser
            _ <- checkParser
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
            <|> shortOfficerMove
            <|> shortPawnTakes
            <|> shortPawnMove

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
    i <- intParser
    _ <- AT.skipWhile (\c -> c == '.' || c == ' ')
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
intParser = AT.decimal

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

1.e4 e5 2.Nf3 d6 3.d4 Bg4 4.dxe5 Bxf3 5.Qxf3 dxe5 6.Bc4 Nf6 7.Qb3 Qe7 8.Nc3 c6 9.Bg5 b5 10.Nxb5 cxb5 11.Bxb5+ Nbd7 12.O-O-O *
|]

-- 1.e4 e5 2.Nf3 d6 3.d4 Bg4 4.dxe5 Bxf3 5.Qxf3 dxe5 6.Bc4 Nf6 7.Qb3 Qe7 8.Nc3 c6 9.Bg5 b5 10.Nxb5 cxb5 11.Bxb5+ Nbd7 12.O-O-O *

actualPgn :: Text
actualPgn =
    [text|
[Event "F/S Return Match"]
[Site "Belgrade, Serbia JUG"]
[Date "1992.11.04"]
[Round "29"]
[White "Fischer, Robert J."]
[Black "Spassky, Boris V."]
[Result "1/2-1/2"]

1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 {This opening is called the Ruy Lopez.}
4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 d6 8. c3 O-O 9. h3 Nb8 10. d4 Nbd7
11. c4 c6 12. cxb5 axb5 13. Nc3 Bb7 14. Bg5 b4 15. Nb1 h6 16. Bh4 c5 17. dxe5
Nxe4 18. Bxe7 Qxe7 19. exd6 Qf6 20. Nbd2 Nxd6 21. Nc4 Nxc4 22. Bxc4 Nb6
23. Ne5 Rae8 24. Bxf7+ Rxf7 25. Nxf7 Rxe1+ 26. Qxe1 Kxf7 27. Qe3 Qg5 28. Qxg5
hxg5 29. b3 Ke6 30. a3 Kd6 31. axb4 cxb4 32. Ra5 Nd5 33. f3 Bc8 34. Kf2 Bf5
35. Ra7 g6 36. Ra6+ Kc5 37. Ke1 Nf4 38. g3 Nxh3 39. Kd2 Kb5 40. Rd6 Kc5 41. Ra6
Nf2 42. g4 Bd3 43. Re6 1/2-1/2    
|]