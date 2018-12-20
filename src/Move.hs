module Move
  ( parseMove
  , parseMoves
  , parseFrom
  , parseTo
  ) where

import           Chess
import           Data.Char
import           Text.Regex.TDFA

parseMove :: String -> Position -> Position
parseMove s pos
  | s =~ "[a-h][1-8].[a-h][1-8]Q" =
    let moveAttempt =
          promoteTo
            (toPlay pos)
            (Chess.movePiece pos (parseFrom s) (parseTo s))
            (Queen White)
        legalMoves = Chess.positionTree pos
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt pos fromSquare)
          then moveAttempt
          else pos
  | s =~ "[a-h][1-8].[a-h][1-8]R" =
    let moveAttempt =
          promoteTo
            (toPlay pos)
            (Chess.movePiece pos (parseFrom s) (parseTo s))
            (Rook White)
        legalMoves = Chess.positionTree pos
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt pos fromSquare)
          then moveAttempt
          else pos
  | s =~ "[a-h][1-8].[a-h][1-8]B" =
    let moveAttempt =
          promoteTo
            (toPlay pos)
            (Chess.movePiece pos (parseFrom s) (parseTo s))
            (Bishop White)
        legalMoves = Chess.positionTree pos
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt pos fromSquare)
          then moveAttempt
          else pos
  | s =~ "[a-h][1-8].[a-h][1-8]K" =
    let moveAttempt =
          promoteTo
            (toPlay pos)
            (Chess.movePiece pos (parseFrom s) (parseTo s))
            (Knight White)
        legalMoves = Chess.positionTree pos
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt pos fromSquare)
          then moveAttempt
          else pos
  | s =~ "[a-h][1-8].[a-h][1-8]" =
    let moveAttempt = Chess.movePiece pos (parseFrom s) (parseTo s)
        legalMoves = Chess.positionTree pos
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if any (eqPosition moveAttempt) legalMoves &&
           colorToPlay == fmap colr (pieceAt pos fromSquare)
          then moveAttempt
          else pos
  | s =~ "O-O-O" =
    let castleAttempt = Chess.castleLong pos (toPlay pos)
        legalMoves = Chess.positionTree pos
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare =
          if toPlay pos == White
            then (Square 5 1)
            else (Square 5 8)
     in if [] /= castleAttempt &&
           head castleAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt pos fromSquare)
          then head castleAttempt
          else pos
  | s =~ "O-O" =
    let castleAttempt = Chess.castleShort pos (toPlay pos)
        legalMoves = Chess.positionTree pos
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare =
          if toPlay pos == White
            then (Square 5 1)
            else (Square 5 8)
     in if [] /= castleAttempt &&
           head castleAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt pos fromSquare)
          then head castleAttempt
          else pos
  | otherwise = pos

parseMoves :: [String] -> Position
parseMoves = foldl (flip parseMove) Chess.startPosition

parseFrom :: String -> Square
parseFrom x = 
  let cC = head x
      c = asInt cC
      rC = x !! 1
      r = digitToInt rC
  in Square c r

parseTo :: String -> Square
parseTo x = 
  let cC = x !! 3
      c = asInt cC
      rC = x !! 4
      r = digitToInt rC
  in Square c r

asInt :: Char -> Int
asInt 'a' = 1
asInt 'b' = 2
asInt 'c' = 3
asInt 'd' = 4
asInt 'e' = 5
asInt 'f' = 6
asInt 'g' = 7
asInt 'h' = 8
asInt x = error $ "not a column I can parse: " ++ [x]