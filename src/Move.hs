module Move
  ( parseMove
  , parseMoves
  , parseFrom
  , parseTo
  ) where

import           Chess
import           Data.Char
import           Text.Regex.TDFA

parseMove :: String -> GameHistory -> GameHistory
parseMove s gh
  | s =~ "[a-h][1-8].[a-h][1-8]Q" =
    let moveAttempt =
          promoteTo
            (toPlay gh)
            (Chess.movePiece (head gh) (parseFrom s) (parseTo s))
            (Queen White)
        legalMoves = Chess.positionTree gh
        colorToPlay =
          if whiteToPlay gh
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt (head gh) fromSquare)
          then moveAttempt : gh
          else gh
  | s =~ "[a-h][1-8].[a-h][1-8]R" =
    let moveAttempt =
          promoteTo
            (toPlay gh)
            (Chess.movePiece (head gh) (parseFrom s) (parseTo s))
            (Rook White)
        legalMoves = Chess.positionTree gh
        colorToPlay =
          if whiteToPlay gh
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt (head gh) fromSquare)
          then moveAttempt : gh
          else gh
  | s =~ "[a-h][1-8].[a-h][1-8]B" =
    let moveAttempt =
          promoteTo
            (toPlay gh)
            (Chess.movePiece (head gh) (parseFrom s) (parseTo s))
            (Bishop White)
        legalMoves = Chess.positionTree gh
        colorToPlay =
          if whiteToPlay gh
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt (head gh) fromSquare)
          then moveAttempt : gh
          else gh
  | s =~ "[a-h][1-8].[a-h][1-8]K" =
    let moveAttempt =
          promoteTo
            (toPlay gh)
            (Chess.movePiece (head gh) (parseFrom s) (parseTo s))
            (Knight White)
        legalMoves = Chess.positionTree gh
        colorToPlay =
          if whiteToPlay gh
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt (head gh) fromSquare)
          then moveAttempt : gh
          else gh
  | s =~ "[a-h][1-8].[a-h][1-8]" =
    let moveAttempt = Chess.movePiece (head gh) (parseFrom s) (parseTo s)
        legalMoves = Chess.positionTree gh
        colorToPlay =
          if whiteToPlay gh
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if any (eqPosition moveAttempt) legalMoves &&
           colorToPlay == fmap colr (pieceAt (head gh) fromSquare)
          then moveAttempt : gh
          else gh
  | s =~ "O-O-O" =
    let castleAttempt = Chess.castleLong gh (toPlay gh)
        legalMoves = Chess.positionTree gh
        colorToPlay =
          if whiteToPlay gh
            then Just White
            else Just Black
        fromSquare =
          if toPlay gh == White
            then (Square 5 1)
            else (Square 5 8)
     in if [] /= castleAttempt &&
           head castleAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt (head gh) fromSquare)
          then (head castleAttempt) : gh
          else gh
  | s =~ "O-O" =
    let castleAttempt = Chess.castleShort gh (toPlay gh)
        legalMoves = Chess.positionTree gh
        colorToPlay =
          if whiteToPlay gh
            then Just White
            else Just Black
        fromSquare =
          if toPlay gh == White
            then (Square 5 1)
            else (Square 5 8)
     in if [] /= castleAttempt &&
           head castleAttempt `elem` legalMoves &&
           colorToPlay == fmap colr (pieceAt (head gh) fromSquare)
          then (head castleAttempt) : gh
          else gh
  | otherwise = gh

parseMoves :: [String] -> GameHistory
parseMoves = foldl (flip parseMove) [Chess.startPosition]

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