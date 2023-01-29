module PGN where

import Board (searchIdx)
import Chess (Status (..), determineStatus, isCheckMate, isInCheck, positionTree)
import Control.Applicative (many, (<|>))
import Control.Monad (join)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AT
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Move (playMoves, squareParser)
import NeatInterpolation
import Position (
    Color (..),
    Move (..),
    Piece (..),
    Position (..),
    Snapshot,
    Square (..),
    colr,
    findMove,
    next,
    pieceAt',
    startPosition,
 )

renderPgn :: Position -> Text
renderPgn pos' =
    let res = renderResult pos'
        mo = renderMoves pos'
     in [text|
        [Event "hChess match"]
        [Site "In front of computer"]
        [Date "2022-12-13"]
        [Round "1"]
        [White "Humanoid Contender"]
        [Black "Computer"]
        [Result "$res"]

        $mo $res
    |]

renderMoves :: Position -> Text
renderMoves Position {..} =
    let indices = repeatEntries [1 ..]
        snapshots = reverse (m : gamehistory) `zip` indices
     in go snapshots
  where
    go [] = ""
    go [(_, i)] = renderNr i i
    go [(from, i), (to, j)] = renderNr i j <> renderMove from to
    go ((from, i) : (to, j) : more) = renderNr i j <> renderMove from to <> " " <> go ((to, j) : more)

renderNr :: Int -> Int -> Text
renderNr i j = if i == j then pack $ show i <> ". " else ""

repeatEntries :: [a] -> [a]
repeatEntries [] = []
repeatEntries (x : xs) = x : x : repeatEntries xs

renderMove :: Snapshot -> Snapshot -> Text
renderMove from to =
    let move' = findMove from to
     in case move' of
            CastleShort -> "O-O"
            CastleLong -> "O-O-O"
            MovedPiece fromSq toSq ->
                let fromPiece' = fromPiece from fromSq
                    fromColor' = maybe (error "PGN missing color in from piece") colr (pieceAt' from fromSq)
                 in renderPiece fromPiece' <> pack (show fromSq) <> renderTakes from to <> pack (show toSq) <> renderCheck to fromColor' -- todo duplication
            EnPassant fromSq toSq ->
                let fromPiece' = fromPiece from fromSq
                    fromColor' = maybe (error "PGN missing color in from piece") colr (pieceAt' from fromSq)
                 in renderPiece fromPiece' <> pack (show fromSq) <> renderTakes from to <> pack (show toSq) <> renderCheck to fromColor' -- todo duplication
            Promotion fromSq toSq piece ->
                let fromPiece' = fromPiece from fromSq
                    fromColor' = maybe (error "PGN missing color in from piece") colr (pieceAt' from fromSq)
                 in renderPiece fromPiece' <> pack (show fromSq) <> renderTakes from to <> pack (show toSq) <> renderProm piece <> renderCheck to fromColor'

renderCheck :: Snapshot -> Color -> Text
renderCheck snp mover =
    let fakePos =
            Position
                { m = snp
                , gamehistory = [m startPosition]
                , pristineShortWhite = True
                , pristineLongWhite = True
                , pristineShortBlack = True
                , pristineLongBlack = True
                , toPlay = next mover
                }
     in if isCheckMate fakePos (positionTree fakePos) then "#" else if isInCheck snp (next mover) then "!" else ""

renderProm :: Piece -> Text
renderProm = (<>) "=" . renderPiece

renderTakes :: Snapshot -> Snapshot -> Text
renderTakes from to =
    if countPieces from White == countPieces to White && countPieces from Black == countPieces to Black then "" else "x"
  where
    countPieces :: Snapshot -> Color -> Int
    countPieces s c = length $ searchIdx s (const True) (\mp -> fmap colr mp == Just c)

renderPiece :: Piece -> Text
renderPiece = \case
    Pawn _ -> ""
    Knight _ -> "N"
    Bishop _ -> "B"
    Rook _ -> "R"
    Queen _ -> "Q"
    King _ -> "K"

fromPiece :: Snapshot -> Square -> Piece
fromPiece snp s = fromMaybe (error "PGN found no fromPiece in snapshot") (pieceAt' snp s)

renderResult :: Position -> Text
renderResult pos = case determineStatus pos (positionTree pos) of
    WhiteIsMate -> "0-1"
    BlackIsMate -> "1-0"
    Remis -> "1/2-1/2"
    _ -> "*"

pgnTester :: IO ()
pgnTester = do
    let Right testPos = playMoves ["e2-e3", "f7-f6", "f2-f4", "g7-g5", "d1-h5"]
    print $ renderPgn testPos

pgnWriteTest :: IO ()
pgnWriteTest = do
    let Right testPos = playMoves ["e2-e3", "f7-f6", "f2-f4", "g7-g5", "d1-h5"]
    TIO.writeFile "game.pgn" (renderPgn testPos)

parsePgn :: Text -> Either String Position
parsePgn = AT.parseOnly $ do
    _ <- many metaLine
    _ <- AT.skipWhile AT.isEndOfLine
    ms <- movesParser
    either
        fail
        pure
        (playMoves (pack . show <$> ms))

data Meta = Meta Text Text
    deriving stock (Show)

metaLines :: Parser [Meta]
metaLines = AT.many1 metaLine

metaLine :: Parser Meta
metaLine = do
    _ <- AT.char '['
    key <- AT.takeTill AT.isHorizontalSpace
    _ <- AT.space
    val <- AT.takeTill (== ']')
    _ <- AT.char ']'
    _ <- AT.endOfLine
    pure $ Meta key val

movesParser :: Parser [Move]
movesParser = do
    whiteblack <- AT.many' whiteBlackParser
    pure $ join whiteblack

pgnMoveParser :: Color -> Parser Move
pgnMoveParser c =
    let castleLongParser = CastleLong <$ AT.string "O-O-O"
        castleShortParser = CastleShort <$ AT.string "O-O"
        promParser = do
            _ <- AT.skipWhile (`elem` ['N', 'B', 'R', 'Q', 'K'])
            fromS <- squareParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.char '='
            piece <- Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K' -- todo Piece should not hold color imo..
            _ <- AT.skipWhile (`elem` ['!', '#'])
            pure $ Promotion fromS toS piece
        regularMoveParser = do
            _ <- AT.skipWhile (`elem` ['N', 'B', 'R', 'Q', 'K'])
            fromS <- squareParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            _ <- AT.skipWhile (`elem` ['!', '#'])
            pure $ MovedPiece fromS toS
     in castleLongParser <|> castleShortParser <|> promParser <|> regularMoveParser

whiteBlackParser :: Parser [Move]
whiteBlackParser = do
    _ <- intParser
    _ <- AT.skipWhile (== '.')
    _ <- AT.space
    withReply <|> withoutReply
  where
    withReply = do
        whiteMove <- pgnMoveParser White
        _ <- AT.space
        blackMove <- pgnMoveParser Black
        _ <- AT.space
        pure [whiteMove, blackMove]
    withoutReply = do
        m <- pgnMoveParser White
        _ <- AT.space
        _ <- result
        pure [m]
    result = AT.string "*" <|> AT.string "1/2-1/2" <|> AT.string "1-0" <|> AT.string "0-1"

intParser :: Parser Int
intParser = fromMaybe 0 . toBoundedInteger <$> AT.scientific

testPgn :: Text
testPgn = [text|
[Event "Paris"]
[Site "Paris FRA"]
[Date "1858.??.??"]
[Round "?"]
[White "Paul Morphy"]
[Black "Duke Karl / Count Isouard"]
[Result "1-0"]

1.e4 e5 2.Nf3 d6 3.d4 Bg4 4.dxe5 Bxf3 5.Qxf3 dxe5 6.Bc4 Nf6 7.Qb3 Qe7
8.Nc3 c6 9.Bg5 b5 10.Nxb5 cxb5 11.Bxb5+ Nbd7 12.O-O-O Rd8
13.Rxd7 Rxd7 14.Rd1 Qe6 15.Bxd7+ *
|]