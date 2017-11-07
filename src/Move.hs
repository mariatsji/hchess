module Move(parseMove) where

import Chess
import Data.Char
import Text.Regex.TDFA

parseMove :: String -> Position -> Position
parseMove s pos
    | s =~ "[a-h][1-8].[a-h][1-8]" = Chess.movePiece pos (parseFrom s) (parseTo s)
    | otherwise = pos

parseFrom x = (head x, digitToInt (x !! 1))
parseTo x = (x !! 3, digitToInt (x !! 4))

