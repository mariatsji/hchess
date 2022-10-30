{-# LANGUAGE OverloadedStrings #-}

module Move (
    parseMove,
    parseMoves,
    parseFrom,
    parseTo,
    debugga
) where

import Chess
import Data.Char
import Position
import Text.Regex.TDFA
import qualified Debug.Trace as Debug

parseMove :: String -> Position -> Either String Position
parseMove s pos
    | s =~ ("[a-h][1-8].[a-h][1-8]Q" :: String) =
        let moveAttempt =
                promoteTo
                    (toPlay pos)
                    (Chess.movePiece pos (parseFrom s) (parseTo s))
                    (parseFrom s)
                    (Queen White)
            fromSquare = parseFrom s
         in if moveAttempt `elem` Chess.positionTree pos
                && Just (toPlay pos)
                    == fmap colr (pieceAt pos fromSquare)
                then Right moveAttempt
                else Left "Could not do queen promotion"
    | s =~ ("[a-h][1-8].[a-h][1-8]R" :: String) =
        let moveAttempt =
                promoteTo
                    (toPlay pos)
                    (Chess.movePiece pos (parseFrom s) (parseTo s))
                    (parseFrom s)
                    (Rook White)
            fromSquare = parseFrom s
         in if moveAttempt `elem` Chess.positionTree pos
                && Just (toPlay pos)
                    == fmap colr (pieceAt pos fromSquare)
                then Right moveAttempt
                else Left "Could not do rook promotion"
    | s =~ ("[a-h][1-8].[a-h][1-8]B" :: String) =
        let moveAttempt =
                promoteTo
                    (toPlay pos)
                    (Chess.movePiece pos (parseFrom s) (parseTo s))
                    (parseFrom s)
                    (Bishop White)
            fromSquare = parseFrom s
         in if moveAttempt `elem` Chess.positionTree pos
                && Just (toPlay pos)
                    == fmap colr (pieceAt pos fromSquare)
                then Right moveAttempt
                else Left "Could not do Bishop promotion"
    | s =~ ("[a-h][1-8].[a-h][1-8]K" :: String) =
        let moveAttempt =
                promoteTo
                    (toPlay pos)
                    (Chess.movePiece pos (parseFrom s) (parseTo s))
                    (parseFrom s)
                    (Knight White)
            fromSquare = parseFrom s
         in if moveAttempt `elem` Chess.positionTree pos
                && Just (toPlay pos)
                    == fmap colr (pieceAt pos fromSquare)
                then Right moveAttempt
                else Left "Could not do Knight promotion"
    | s =~ ("[a-h][1-8].[a-h][1-8]" :: String) =
        let moveAttempt = Chess.movePiece pos (parseFrom s) (parseTo s)
            fromSquare = parseFrom s
            isAmongLegalMoves = any (eqPosition moveAttempt) (Chess.positionTree pos)
            correctColorMadeTheMove = Just (toPlay pos) == fmap colr (pieceAt pos fromSquare)
         in if Debug.traceShowId isAmongLegalMoves
                && correctColorMadeTheMove
                then Right moveAttempt
                else Left "Could not find move to be OK"
    | s =~ ("O-O-O" :: String) =
        let castleAttempt = Chess.castleLong pos (toPlay pos)
            fromSquare =
                if toPlay pos == White
                    then Square 5 1
                    else Square 5 8
         in if [] /= castleAttempt
                && head castleAttempt
                    `elem` Chess.positionTree pos
                && Just (toPlay pos)
                    == fmap colr (pieceAt pos fromSquare)
                then Right $ head castleAttempt
                else Left "Could not do O-O-O"
    | s =~ ("O-O" :: String) =
        let castleAttempt = Chess.castleShort pos (toPlay pos)
            fromSquare =
                if toPlay pos == White
                    then Square 5 1
                    else Square 5 8
         in if [] /= castleAttempt
                && head castleAttempt
                    `elem` positionTree pos
                && Just (toPlay pos)
                    == fmap colr (pieceAt pos fromSquare)
                then Right $ head castleAttempt
                else Left "Could not do O-O"
    | otherwise = Left $ "Unable to parse move with string " ++ s

parseMoves :: [String] -> Either String Position
parseMoves =
    foldl
        (\acc c -> acc >>= parseMove c)
        (Right startPosition :: Either String Position)

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

debugga :: IO ()
debugga = do
  let (Right pos) = parseMoves ["e2-e4", "e7-e5", "f1-c4", "b8-c6", "d1-h5", "g8-f6"] --, "h5-f7"]
  print $ length $ positionTreeIgnoreCheck pos
  print $ head $ positionTreeIgnoreCheck pos
  