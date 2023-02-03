module PGN where

import Board (searchIdx)
import Chess (Status (..), determineStatus, isCheckMate, isInCheck, movePiece, playIfLegal, positionTree)
import Control.Applicative (many, (<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AT
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Move (colParser, playMoves, rowParser, squareParser)
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
    searchForPieces,
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

roll :: a -> (a -> Parser a) -> Parser a
roll seed p = do
    new <- p seed
    (new <$ AT.endOfInput) <|> roll new p

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
            either
                fail
                pure
                (playIfLegal CastleLong pos)
        castleShortParser = do
            _ <- AT.string "O-O"
            either
                fail
                pure
                (playIfLegal CastleShort pos)
        promParser = do
            -- c7-c8=Q  Bc7c8#
            _ <- AT.skipWhile (`elem` ['N', 'B', 'R', 'Q', 'K'])
            fromS <- squareParser
            _ <- AT.skipWhile (\ch -> ch == 'x' || ch == '-')
            toS <- squareParser
            _ <- AT.char '='
            piece <- pieceParser c
            let move = Promotion fromS toS piece
            either
                fail
                pure
                (playIfLegal move pos)
        shortPawnTakesPromParser = do
            -- bxc8=K
            fromCol <- colParser
            _ <- AT.skipWhile (\ch -> ch == 'x' || ch == '-')
            toS <- squareParser
            _ <- AT.char '='
            promPiece <- pieceParser c
            let fromS = before toS {col = fromCol} c
                piece = Pawn c
                squarePred = (==) fromS 
                move = Promotion fromS toS promPiece
            either
                fail
                pure
                ( findPiece pos piece squarePred toS
                    >>= (\_ -> playIfLegal move pos)
                )
        shortPromParser = do
            -- e8=Q
            toS <- squareParser
            _ <- AT.char '='
            promPiece <- pieceParser c
            let fromS = before toS c
                piece = Pawn c
                squarePred = (==) fromS 
                move = Promotion fromS toS promPiece
            either
                fail
                pure
                ( findPiece pos piece squarePred toS
                    >>= (\_ -> playIfLegal move pos)
                )
        regularOfficerMoveParser = do
            -- Qd1g4
            _ <- pieceParser c
            fromS <- squareParser
            _ <- AT.skipWhile (\ch -> ch == 'x' || ch == '-')
            toS <- squareParser
            let move = MovedPiece fromS toS
            either
                fail
                pure
                (playIfLegal move pos)
        regularPawnMoveParser = do
            -- e1-e2
            fromS <- squareParser
            _ <- AT.skipWhile (\ch -> ch == 'x' || ch == '-')
            toS <- squareParser
            let move = MovedPiece fromS toS
            either
                fail
                pure
                (playIfLegal move pos)
        shortOfficerColMove = do
            -- Kdc4 / Kdxc4
            piece <- pieceParser c
            fromCol <- colParser
            _ <- AT.skipWhile (\ch -> ch == 'x' || ch == '-')
            toS <- squareParser
            let squarePred (Square col' _) = col' == fromCol
            either
                fail
                pure
                ( findPiece pos piece squarePred toS
                    >>= (\fromS -> playIfLegal (MovedPiece fromS toS) pos)
                )
        shortOfficerRowMove = do
            -- K5c5 / K5xc5
            piece <- pieceParser c
            fromRow <- rowParser
            _ <- AT.skipWhile (== 'x')
            toS <- squareParser
            let squarePred (Square _ row') = row' == fromRow
            either
                fail
                pure
                ( findPiece pos piece squarePred toS
                    >>= (\fromS -> playIfLegal (MovedPiece fromS toS) pos)
                )
        shortOfficerMove = do
            -- Kc4 / Kxc4
            piece <- pieceParser c
            _ <- AT.skipWhile (\ch -> ch == 'x' || ch == '-')
            toS <- squareParser
            let squarePred = const True
            either
                fail
                pure
                ( findPiece pos piece squarePred toS
                    >>= (\fromS -> playIfLegal (MovedPiece fromS toS) pos)
                )
        shortPawnTakes = do
            -- bxc4
            fromCol <- colParser
            _ <- AT.skipWhile (\ch -> ch == 'x' || ch == '-')
            toS <- squareParser
            let piece = Pawn c
                squarePred (Square col' _) = col' == fromCol
            either
                fail
                pure
                ( findPiece pos piece squarePred toS
                    >>= (\fromS -> playIfLegal (MovedPiece fromS toS) pos)
                )
        shortPawnMove = do
            -- e4
            toS@(Square toCol _) <- squareParser
            let piece = Pawn c
                squarePred (Square col' _) = col' == toCol
            either
                fail
                pure
                ( findPiece pos piece squarePred toS
                    >>= (\fromS -> playIfLegal (MovedPiece fromS toS) pos)
                )
     in ( castleLongParser
            <|> castleShortParser
            <|> promParser
            <|> shortPawnTakesPromParser
            <|> shortPromParser
            <|> regularOfficerMoveParser
            <|> regularPawnMoveParser
            <|> shortOfficerColMove
            <|> shortOfficerRowMove
            <|> shortOfficerMove
            <|> shortPawnTakes
            <|> shortPawnMove
        )
            <* checkParser

pieceParser :: Color -> Parser Piece
pieceParser c = Knight c <$ AT.char 'N' <|> Bishop c <$ AT.char 'B' <|> Rook c <$ AT.char 'R' <|> Queen c <$ AT.char 'Q' <|> King c <$ AT.char 'K'

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
    (pos <$ result) <|> withReplyTerminated <|> withoutReplyTerminated <|> withReply <|> withoutReply
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

longPgn :: Text
longPgn =
    [text|
[Event "hChess match"]
[Site "In front of computer"]
[Date "2022-12-13"]
[Round "1"]
[White "Humanoid Contender"]
[Black "Computer"]
[Result "1-0"]

1. e2e3 e7e6 2. Qd1g4 Qd8f6 3. Nb1c3 h7h5 4. Qg4h3 Nb8c6 5. Bf1b5 Nc6a5 6. a2a3 Qf6e5 7. b2b4 Bf8xb4 8. a3xb4 b7b6 9. b4xa5 b6xa5 10. Ra1xa5 c7c6
11. Ng1f3 Qe5c5 12. Bc1a3 Qc5b6 13. Bb5xc6 Qb6xc6 14. Ra5c5 Qc6d6 15. Rc5xc8! Ra8xc8 16. Ba3xd6 Rc8c6 17. Bd6a3 Ng8f6 18. Ke1e2 Nf6g4 19. Nf3d4 Rc6c4
20. Ke2d3 Ng4e5! 21. Kd3e4 f7f6 22. Ba3d6 Ne5f7 23. Bd6a3 Nf7g5 24. Ke4d3 Rc4xc3! 25. d2xc3 Ng5xh3 26. g2xh3 e6e5 27. Nd4b5 a7a6 28. Nb5c7! Ke8f7
29. Nc7xa6 Rh8a8 30. f2f4 Ra8xa6 31. Ba3c5 d7d6 32. Bc5b4 Ra6b6 33. Bb4a5 Rb6b5 34. Ba5b4 e5e4! 35. Kd3xe4 d6d5! 36. Ke4d4 g7g5 37. Rh1a1 g5xf4
38. e3xf4 Rb5b7 39. Kd4xd5 Rb7b5! 40. Bb4c5 h5h4 41. Ra1a6 Kf7g6 42. c3c4 Rb5b2 43. Bc5d4 Rb2xc2 44. Ra6xf6! Kg6h5 45. Rf6f5! Kh5h6 46. Rf5f6!
Kh6h5 47. Rf6f5! Kh5h6 48. Rf5f6! Kh6h5 49. Rf6f5! Kh5h6 50. Bd4f6 Rc2xh2 51. Bf6g5! Kh6g6 52. Rf5f6! Kg6g7 53. Bg5xh4 Rh2xh3 54. Bh4g5 Rh3h5
55. Rf6a6 Rh5h3 56. Bg5f6! Kg7f7 57. Ra6e6 Rh3h6 58. f4f5 Rh6xf6 59. Re6e5 Kf7f8 60. c4c5 Rf6f7 61. c5c6 Rf7f6 62. c6c7 Rf6d6! 63. Kd5xd6 Kf8f7
64. c7c8=Q Kf7f6 65. Qc8c4 Kf6g5 66. Kd6d7 Kg5f6 67. Kd7d6 Kf6g5 68. Kd6d7 Kg5f6 69. Kd7d6 Kf6g5 70. Kd6d7 Kg5f6 71. Re5a5 Kf6g5 72. Kd7e7 Kg5h5
73. Ra5a2 Kh5g5 74. Ke7e6 Kg5h5 75. Ke6f7 Kh5g5 76. Qc4e4 Kg5h5 77. Kf7g8 Kh5g5 78. Kg8g7 Kg5h5 79. f5f6 Kh5g5 80. Ra2a5# 1-0
|]

enPassant :: Text
enPassant =
    [text|
[Event "hChess match"]
[Site "In front of computer"]
[Date "2022-12-13"]
[Round "1"]
[White "Humanoid Contender"]
[Black "Computer"]
[Result "*"]

1. b2b4 h7h5 2. b4b5 a7a5 3. b5xa6 *
|]

startPos :: Text
startPos =
    [text|
[Event "hChess match"]
[Site "In front of computer"]
[Date "2022-12-13"]
[Round "1"]
[White "Humanoid Contender"]
[Black "Computer"]
[Result "*"]

1. *
|]

immortalGame :: Text
immortalGame =
    [text|
[Event "Casual game"]
[Site "London ENG"]
[Date "1851.06.21"]
[EventDate "?"]
[Round "?"]
[Result "1-0"]
[White "Adolf Anderssen"]
[Black "Lionel Adalbert Bagration Felix Kieseritzky"]
[ECO "C33"]
[WhiteElo "?"]
[BlackElo "?"]
[Source "The Chess Player, vol.i no.1, 1851.07.19, p.2"]
[PlyCount "45"]

1.e4 e5 2.f4 exf4 3.Bc4 Qh4+ 4.Kf1 b5 5.Bxb5 Nf6 6.Nf3 Qh6
7.d3 Nh5 8.Nh4 Qg5 9.Nf5 c6 10.g4 Nf6 11.Rg1 cxb5 12.h4 Qg6
13.h5 Qg5 14.Qf3 Ng8 15.Bxf4 Qf6 16.Nc3 Bc5 17.Nd5 Qxb2 18.Bd6
Bxg1 {It is from this move that Black's defeat stems. Wilhelm
Steinitz suggested in 1879 that a better move would be
18... Qxa1+; likely moves to follow are 19. Ke2 Qb2 20. Kd2
Bxg1.} 19. e5 Qxa1+ 20. Ke2 Na6 21.Nxg7+ Kd8 22.Qf6+ Nxf6
23.Be7# 1-0
|]