{-# LANGUAGE OverloadedStrings #-}

module Move
  ( parseMove,
    parseMoves,
    parseFrom,
    parseTo
    )
where

import Chess
import Data.Char
import Text.Regex.TDFA

parseMove :: String -> Position -> Either String Position
parseMove s pos
  | s =~ ("[a-h][1-8].[a-h][1-8]Q" :: String) =
    let moveAttempt =
          promoteTo
            (toPlay pos)
            (Chess.movePiece pos (parseFrom s) (parseTo s))
            (Queen White)
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` Chess.positionTree pos
          && colorToPlay
          == fmap colr (pieceAt pos fromSquare)
          then Right moveAttempt
          else Left "Could not do queen promotion"
  | s =~ ("[a-h][1-8].[a-h][1-8]R" :: String) =
    let moveAttempt =
          promoteTo
            (toPlay pos)
            (Chess.movePiece pos (parseFrom s) (parseTo s))
            (Rook White)
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` Chess.positionTree pos
          && colorToPlay
          == fmap colr (pieceAt pos fromSquare)
          then Right moveAttempt
          else Left "Could not do rook promotion"
  | s =~ ("[a-h][1-8].[a-h][1-8]B" :: String) =
    let moveAttempt =
          promoteTo
            (toPlay pos)
            (Chess.movePiece pos (parseFrom s) (parseTo s))
            (Bishop White)
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` Chess.positionTree pos
          && colorToPlay
          == fmap colr (pieceAt pos fromSquare)
          then Right moveAttempt
          else Left "Could not do Bishop promotion"
  | s =~ ("[a-h][1-8].[a-h][1-8]K" :: String) =
    let moveAttempt =
          promoteTo
            (toPlay pos)
            (Chess.movePiece pos (parseFrom s) (parseTo s))
            (Knight White)
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if moveAttempt `elem` Chess.positionTree pos
          && colorToPlay
          == fmap colr (pieceAt pos fromSquare)
          then Right moveAttempt
          else Left "Could not do Knight promotion"
  | s =~ ("[a-h][1-8].[a-h][1-8]" :: String) =
    let moveAttempt = Chess.movePiece pos (parseFrom s) (parseTo s)
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare = parseFrom s
     in if any (eqPosition moveAttempt) (Chess.positionTree pos)
          && colorToPlay
          == fmap colr (pieceAt pos fromSquare)
          then Right moveAttempt
          else Left "Could not find move to be OK"
  | s =~ ("O-O-O" :: String) =
    let castleAttempt = Chess.castleLong pos (toPlay pos)
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare =
          if toPlay pos == White
            then Square 5 1
            else Square 5 8
     in if [] /= castleAttempt
          && head castleAttempt
          `elem` Chess.positionTree pos
          && colorToPlay
          == fmap colr (pieceAt pos fromSquare)
          then Right $ head castleAttempt
          else Left "Could not do O-O-O"
  | s =~ ("O-O" :: String) =
    let castleAttempt = Chess.castleShort pos (toPlay pos)
        colorToPlay =
          if whiteToPlay pos
            then Just White
            else Just Black
        fromSquare =
          if toPlay pos == White
            then Square 5 1
            else Square 5 8
     in if [] /= castleAttempt
          && head castleAttempt
          `elem` Chess.positionTree pos
          && colorToPlay
          == fmap colr (pieceAt pos fromSquare)
          then Right $ head castleAttempt
          else Left "Could not do O-O"
  | otherwise = Left $ "Unable to parse move with string " ++ s

parseMoves :: [String] -> Either String Position
parseMoves moves =
  foldr
    (\s a -> a >>= parseMove s)
    (Right Chess.startPosition :: Either String Position)
    (reverse moves)

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
