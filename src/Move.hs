module Move (
    playMove,
    playMoves,
    parsedMove,
    squareParser,
    colParser,
    rowParser
) where

import Chess (playIfLegal)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, parseOnly, string)
import Data.Text (Text)
import Position (
    Col,
    Color,
    Move (..),
    Piece (Bishop, Knight, Queen, Rook),
    Position (toPlay),
    Row,
    Square (Square),
    startPosition,
 )

parsedMove :: Position -> Text -> Either String Move
parsedMove pos = parseOnly $ moveParser pos

moveParser :: Position -> Parser Move
moveParser pos = promParser pos <|> castleParser <|> regularMoveParser

regularMoveParser :: Parser Move
regularMoveParser = do
    from <- squareParser
    _ <- char '-'
    MovedPiece from <$> squareParser

castleParser :: Parser Move
castleParser = castleLongParser <|> castleShortParser
  where
    castleLongParser = CastleLong <$ string "O-O-O"
    castleShortParser = CastleShort <$ string "O-O"

promParser :: Position -> Parser Move
promParser pos = do
    from <- squareParser
    _ <- char '-'
    to <- squareParser
    Promotion from to <$> pieceParser (toPlay pos)

squareParser :: Parser Square
squareParser =
    Square <$> colParser <*> rowParser

colParser :: Parser Col
colParser = colAparser <|> colBparser <|> colCparser <|> colDparser <|> colEparser <|> colFparser <|> colGparser <|> colHparser
  where
    colAparser = 1 <$ char 'a'
    colBparser = 2 <$ char 'b'
    colCparser = 3 <$ char 'c'
    colDparser = 4 <$ char 'd'
    colEparser = 5 <$ char 'e'
    colFparser = 6 <$ char 'f'
    colGparser = 7 <$ char 'g'
    colHparser = 8 <$ char 'h'

rowParser :: Parser Row
rowParser = rowAparser <|> rowBparser <|> rowCparser <|> rowDparser <|> rowEparser <|> rowFparser <|> rowGparser <|> rowHparser
  where
    rowAparser = 1 <$ char '1'
    rowBparser = 2 <$ char '2'
    rowCparser = 3 <$ char '3'
    rowDparser = 4 <$ char '4'
    rowEparser = 5 <$ char '5'
    rowFparser = 6 <$ char '6'
    rowGparser = 7 <$ char '7'
    rowHparser = 8 <$ char '8'

pieceParser :: Color -> Parser Piece
pieceParser color = knightParser <|> bishopParser <|> rookParser <|> queenParser
  where
    knightParser = Knight color <$ char 'K'
    bishopParser = Bishop color <$ char 'B'
    rookParser = Rook color <$ char 'R'
    queenParser = Queen color <$ char 'Q'

playMove :: Text -> Position -> Either String Position
playMove s pos = do
    move <- parseOnly (moveParser pos) s
    playIfLegal move pos

playMoves :: [Text] -> Either String Position
playMoves =
    foldl
        (\acc c -> acc >>= playMove c)
        (Right startPosition :: Either String Position)
