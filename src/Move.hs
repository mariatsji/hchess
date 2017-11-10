module Move(parseMove) where

import Chess
import Data.Char
import Text.Regex.TDFA

parseMove :: String -> [Position] -> [Position]
parseMove s pos
    | s =~ "[a-h][1-8].[a-h][1-8]" =
            let moveAttempt = Chess.movePiece (head pos) (parseFrom s) (parseTo s)
                legalMoves = Chess.positionTree pos
                colorToPlay = if whiteToPlay pos then Just White else Just Black
                fromSquare = parseFrom s
            in if moveAttempt `elem` legalMoves && colorToPlay == fmap color (pieceAt (head pos) fromSquare) then moveAttempt : pos else pos
    | otherwise = pos

parseFrom x = (head x, digitToInt (x !! 1))
parseTo x = (x !! 3, digitToInt (x !! 4))

