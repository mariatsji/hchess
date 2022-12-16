module PGN where

import Board (searchIdx)
import Chess (Status (..), determineStatus, isCheckMate, isInCheck, positionTree)
import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AT
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Debug.Trace as Debug
import Move (playMoves, squareParser)
import NeatInterpolation
import Position (
    CastleStatus (CanCastleBoth),
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
import Printer (pretty)

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
        [Result = "$res"]

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
                , castleStatusWhite = CanCastleBoth
                , castleStatusBlack = CanCastleBoth
                , toPlay = next mover
                }
     in if isCheckMate fakePos (positionTree fakePos) then "#" else if isInCheck fakePos then "!" else ""

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
renderResult pos = case determineStatus pos of
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
parsePgn = AT.parseOnly pgnParser
  where
    pgnParser = do
        _ <- AT.many' metaLine
        _ <- AT.endOfLine
        ms <- movesParser
        either
            fail
            pure
            (Debug.trace (show (length ms)) $ playMoves (pack . show <$> ms))
    metaLine = AT.char '[' *> AT.manyTill' AT.anyChar (AT.char ']')

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

testParse :: IO ()
testParse = do
    content <- TIO.readFile "movebug.pgn"
    let pos = parsePgn content
    print pos
    -- pretty pos

testMovesParser :: IO ()
testMovesParser = do
    let movesString = "1. e2e3 d7d5 2. Qd1h5 Qd8d6 3. Bf1b5! Bc8d7 4. Bb5d3 g7g6 5. Qh5g5 Bf8h6 6. Qg5h4 Qd6f6 7. Qh4b4 Qf6b6 8. Qb4c3 d5d4 9. e3xd4 Qb6c6 10. Qc3xc6 Bd7xc6 11. f2f3 Nb8d7 12. Nb1c3 Nd7b6 13. d4d5 Nb6xd5 14. Bd3xg6 h7xg6 15. Ng1e2 Ng8f6 16. d2d3 Bh6xc1 17. Ra1xc1 Nd5xc3 18. Ne2xc3 O-O-O 19. Ke1f2 Nf6d5 20. Nc3e4 Nd5b4 21. Ne4c3 Nb4d5 22. Nc3e4 Nd5b4 23. Ne4c3 Nb4d5 24. Nc3e4 Nd5b4 25. Ne4c3 b7b6 26. a2a3 Nb4d5 27. Rc1b1 Kc8b7 28. Rh1e1 Nd5xc3 29. b2xc3 Rh8xh2 30. Re1xe7 Rh2h7 31. Rb1e1 Rd8d7 32. Re7e5 Rh7h5 33. Re5xh5 g6xh5 34. Re1h1 Bc6a4 35. Rh1xh5 Ba4xc2 36. Rh5b5 Rd7xd3 37. Kf2g3 Rd3xc3 38. Rb5b2 Rc3c6 39. Kg3f2 Bc2d3 40. f3f4 Rc6c2! 41. Rb2xc2 Bd3xc2 42. g2g4 Bc2d3 43. f4f5 f7f6 44. Kf2g3 Bd3e4 45. Kg3h4 Kb7a6 46. g4g5 Be4xf5 47. g5xf6 Bf5g6 48. Kh4h3 Ka6a5 49. Kh3h4 Ka5a4 50. Kh4g5 Bg6f7 51. Kg5h6 *"
    print $ AT.parseOnly movesParser movesString

testSmall :: IO ()
testSmall = do
    let movesString = "d7d5"
    print $ AT.parseOnly (pgnMoveParser White) movesString

testWhiteBlack :: IO ()
testWhiteBlack = do
    let s = "1. e2e3 d7d5"
    print $ AT.parseOnly whiteBlackParser s