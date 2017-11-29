module Move(parseMove) where

import Chess
import Data.Char
import Text.Regex.TDFA

parseMove :: String -> [Position] -> [Position]
parseMove s pos
    | s =~ "[a-h][1-8].[a-h][1-8]Q" =
            let moveAttempt = promoteTo (toPlay pos) (Chess.movePiece (head pos) (parseFrom s) (parseTo s)) (Queen White)
                legalMoves = Chess.positionTree pos
                colorToPlay = if whiteToPlay pos then Just White else Just Black
                fromSquare = parseFrom s
            in if moveAttempt `elem` legalMoves && colorToPlay == fmap color (pieceAt (head pos) fromSquare) then moveAttempt : pos else pos
    | s =~ "[a-h][1-8].[a-h][1-8]R" =
            let moveAttempt = promoteTo (toPlay pos) (Chess.movePiece (head pos) (parseFrom s) (parseTo s)) (Rook White)
                legalMoves = Chess.positionTree pos
                colorToPlay = if whiteToPlay pos then Just White else Just Black
                fromSquare = parseFrom s
            in if moveAttempt `elem` legalMoves && colorToPlay == fmap color (pieceAt (head pos) fromSquare) then moveAttempt : pos else pos
    | s =~ "[a-h][1-8].[a-h][1-8]B" =
            let moveAttempt = promoteTo (toPlay pos) (Chess.movePiece (head pos) (parseFrom s) (parseTo s)) (Bishop White)
                legalMoves = Chess.positionTree pos
                colorToPlay = if whiteToPlay pos then Just White else Just Black
                fromSquare = parseFrom s
            in if moveAttempt `elem` legalMoves && colorToPlay == fmap color (pieceAt (head pos) fromSquare) then moveAttempt : pos else pos
    | s =~ "[a-h][1-8].[a-h][1-8]B" =
            let moveAttempt = promoteTo (toPlay pos) (Chess.movePiece (head pos) (parseFrom s) (parseTo s)) (Knight White)
                legalMoves = Chess.positionTree pos
                colorToPlay = if whiteToPlay pos then Just White else Just Black
                fromSquare = parseFrom s
            in if moveAttempt `elem` legalMoves && colorToPlay == fmap color (pieceAt (head pos) fromSquare) then moveAttempt : pos else pos
    | s =~ "[a-h][1-8].[a-h][1-8]" =
            let moveAttempt = Chess.movePiece (head pos) (parseFrom s) (parseTo s)
                legalMoves = Chess.positionTree pos
                colorToPlay = if whiteToPlay pos then Just White else Just Black
                fromSquare = parseFrom s
            in if moveAttempt `elem` legalMoves && colorToPlay == fmap color (pieceAt (head pos) fromSquare) then moveAttempt : pos else pos
    | s =~ "O-O" =
            let castleAttempt = Chess.castleShort pos (toPlay pos)
                legalMoves = Chess.positionTree pos
                colorToPlay = if whiteToPlay pos then Just White else Just Black
                fromSquare = if (toPlay pos) == White then ('e', 1) else ('e', 8)
            in if [] /= castleAttempt && (head castleAttempt) `elem` legalMoves && colorToPlay == fmap color (pieceAt (head pos) fromSquare) then (head castleAttempt) : pos else pos
    | s =~ "O-O-O" =
            let castleAttempt = Chess.castleLong pos (toPlay pos)
                legalMoves = Chess.positionTree pos
                colorToPlay = if whiteToPlay pos then Just White else Just Black
                fromSquare = if (toPlay pos) == White then ('e', 1) else ('e', 8)
            in if [] /= castleAttempt && (head castleAttempt) `elem` legalMoves && colorToPlay == fmap color (pieceAt (head pos) fromSquare) then (head castleAttempt) : pos else pos
    | otherwise = pos

parseFrom x = (head x, digitToInt (x !! 1))
parseTo x = (x !! 3, digitToInt (x !! 4))

