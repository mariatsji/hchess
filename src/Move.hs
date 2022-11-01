{-# LANGUAGE OverloadedStrings #-}

module Move (
    parseMove,
    parseMoves,
    parseFrom,
    parseTo,
) where

import Chess
import Data.Char
import Position
import Text.Regex.TDFA

parseMove :: String -> Position -> Either String Position
parseMove s pos
    | s =~ ("[a-h][1-8].[a-h][1-8]Q" :: String) =
        let promPiece = if toPlay pos == White then Queen White else Queen Black
            moveAttempt = Chess.movePiecePromote pos (parseFrom s) (parseTo s) promPiece
            fromSquare = parseFrom s
            isAmongLegalMoves = any (eqPosition moveAttempt) (Chess.positionTree pos)
            correctColorMadeTheMove = Just (toPlay pos) == fmap colr (pieceAt pos fromSquare)
         in if isAmongLegalMoves && correctColorMadeTheMove
                then Right moveAttempt
                else Left "Could not find Queen prom move to be OK"
    | s =~ ("[a-h][1-8].[a-h][1-8]R" :: String) =
        let promPiece = if toPlay pos == White then Rook White else Rook Black
            moveAttempt = Chess.movePiecePromote pos (parseFrom s) (parseTo s) promPiece
            fromSquare = parseFrom s
            isAmongLegalMoves = any (eqPosition moveAttempt) (Chess.positionTree pos)
            correctColorMadeTheMove = Just (toPlay pos) == fmap colr (pieceAt pos fromSquare)
         in if isAmongLegalMoves && correctColorMadeTheMove
                then Right moveAttempt
                else Left "Could not find Rook prom move to be OK"
    | s =~ ("[a-h][1-8].[a-h][1-8]B" :: String) =
        let promPiece = if toPlay pos == White then Bishop White else Bishop Black
            moveAttempt = Chess.movePiecePromote pos (parseFrom s) (parseTo s) promPiece
            fromSquare = parseFrom s
            isAmongLegalMoves = any (eqPosition moveAttempt) (Chess.positionTree pos)
            correctColorMadeTheMove = Just (toPlay pos) == fmap colr (pieceAt pos fromSquare)
         in if isAmongLegalMoves && correctColorMadeTheMove
                then Right moveAttempt
                else Left "Could not find Bishop prom move to be OK"
    | s =~ ("[a-h][1-8].[a-h][1-8]K" :: String) =
        let promPiece = if toPlay pos == White then Knight White else Knight Black
            moveAttempt = Chess.movePiecePromote pos (parseFrom s) (parseTo s) promPiece
            fromSquare = parseFrom s
            isAmongLegalMoves = any (eqPosition moveAttempt) (Chess.positionTree pos)
            correctColorMadeTheMove = Just (toPlay pos) == fmap colr (pieceAt pos fromSquare)
         in if isAmongLegalMoves && correctColorMadeTheMove
                then Right moveAttempt
                else Left "Could not find Knight prom move to be OK"
    | s =~ ("[a-h][1-8].[a-h][1-8]" :: String) =
        let moveAttempt = Chess.movePiece pos (parseFrom s) (parseTo s)
            fromSquare = parseFrom s
            tree = Chess.positionTree pos
            isAmongLegalMoves = any (eqPosition moveAttempt) tree
            correctColorMadeTheMove = Just (toPlay pos) == fmap colr (pieceAt pos fromSquare)
         in if null tree then
                Left "No legal moves, empty positionTree prior to move"
            else if not isAmongLegalMoves then
                Left$  "Move not among " <> show (length tree) <> " legal moves"
            else if not correctColorMadeTheMove then
                Left "Move made by incorrect color"
            else Right moveAttempt
    | s =~ ("O-O-O" :: String) =
        let castleAttempt = Chess.castleLong pos
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
        let castleAttempt = Chess.castleShort pos
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
