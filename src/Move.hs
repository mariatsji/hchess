module Move
  ( playMove,
    playMoves,
    parsedMove,
    squareParser,
    colParser,
    rowParser,
  )
where

import Chess (playIfLegal)
import Data.Attoparsec.Text (Parser, char, parseOnly, string)
import Data.Foldable (foldl)
import Position
  ( Col,
    Color,
    Move (..),
    Piece (Bishop, Knight, Queen, Rook),
    Position,
    Row,
    Square (Square),
    startPosition,
    toPlay,
  )
import Relude

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
colParser = asum [i <$ char c | (c, i) <- zip ['a'..'h'] [1..8]]

rowParser :: Parser Row
rowParser = asum [i <$ char c | (c, i) <- zip ['1'..'8'] [1..8]]

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
