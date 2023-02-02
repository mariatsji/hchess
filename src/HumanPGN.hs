module HumanPGN (parsePgn) where

import Chess (pieceAt, playIfLegal)
import Control.Applicative (many, (<|>))
import Control.Monad (join, foldM)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AT
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text, pack)
import Move (playMoves, squareParser, colParser)
import NeatInterpolation
import PGN (metaLine)
import Position (Color (..), Move (CastleLong, CastleShort, MovedPiece, Promotion), Piece (..), Position (..), Square (..), startPosition, fromLetter)
import GHC.Base (MonadPlus)
import Control.Monad (mzero)

roll :: MonadPlus m => a -> [a -> m a] -> m a
roll = foldM (\ a x -> x a)


parsePgn :: Text -> Either String Position
parsePgn = AT.parseOnly
    (do
        _ <- many metaLine
        _ <- AT.skipWhile AT.isEndOfLine
        movesParser
    )

movesParser :: Parser Position
movesParser = do
    let list = replicate 1000 whiteBlackParser
    roll startPosition list

pgnMoveParser :: Position -> Color -> Parser Position
pgnMoveParser pos c =
    let castleLongParser = do
            _ <- AT.string "O-O-O"
            case playIfLegal CastleLong pos of
                Right newPos -> pure newPos
                Left s -> fail s
        castleShortParser = do
            _ <- AT.string "O-O"
            case playIfLegal CastleShort pos of
                Right newPos -> pure newPos
                Left s -> fail s
        promParser = do -- c7-c8=Q
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
        shortPromParser = do -- e8=Q
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
        shortPawnTakesPromParser = do -- bxc8=K
            fromCol <- colParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.char '='
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            _ <- AT.skipWhile (`elem` ['!', '#'])
            let fromS = before toS { col = fromCol } c
            if pieceAt pos fromS == Just (Pawn c)
                then
                    let move = Promotion fromS toS piece
                    in case playIfLegal move pos of
                        Right newPos -> pure newPos
                        Left s -> fail s
                else fail $ "No short promotion when unexpected pawn at " <> show fromS
        regularMoveParser = do -- e1-e2
            _ <- AT.skipWhile (`elem` ['N', 'B', 'R', 'Q', 'K'])
            fromS <- squareParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.skipWhile (`elem` ['!', '#'])
            case playIfLegal (MovedPiece fromS toS) pos of
                        Right newPos -> pure newPos
                        Left s -> fail s
        shortOfficerMove = do -- Kc4 / Kxc4
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            fromCol <- colParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.skipWhile (`elem` ['!', '#'])
            -- search for fromSquare! aiai
        shortOfficerColMove = undefined -- Kdc4 / Kdxc4
        shortOfficerRowMove = undefined  -- K5c5 / K5xc5
        shortPawnTakes = undefined -- bxc4
        shortPawnMove = undefined -- e4

     in castleLongParser
            <|> castleShortParser
            <|> promParser
            <|> shortPromParser
            <|> shortPawnTakesPromParser
            <|> regularMoveParser


before :: Square -> Color -> Square
before (Square c r) White = Square (c - 1) r
before (Square c r) Black = Square (c + 1) r

whiteBlackParser :: Position -> Parser Position
whiteBlackParser pos = do
    _ <- intParser
    _ <- AT.skipWhile (== '.')
    _ <- AT.space
    withReply <|> withoutReply
  where
    withReply = do
        whiteMove <- pgnMoveParser pos White
        _ <- AT.space
        blackMove <- pgnMoveParser whiteMove Black
        _ <- AT.space
        pure blackMove
    withoutReply = do
        po <- pgnMoveParser pos White
        _ <- AT.space
        _ <- result
        pure po
    result = AT.string "*" <|> AT.string "1/2-1/2" <|> AT.string "1-0" <|> AT.string "0-1"

intParser :: Parser Int
intParser = fromMaybe 0 . toBoundedInteger <$> AT.scientific

testPgn :: Text
testPgn =
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

1.e4 e5 2.Nf3 d6 3.d4 Bg4 4.dxe5 Bxf3 5.Qxf3 dxe5 6.Bc4 Nf6 7.Qb3 Qe7 8.Nc3 c6 9.Bg5  b5 10.Nxb5 cxb5 11.Bxb5+ Nbd7
12.O-O-O Rd8 13.Rxd7 Rxd7 14.Rd1 Qe6 15.Bxd7+ Nxd7 16.Qb8+ Nxb8 17.Rd8# 1-0
|]