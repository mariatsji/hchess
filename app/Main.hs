module Main where

import Chess
import Data.Char
import Printer
import System.IO
import Text.Regex.TDFA


main :: IO ()
main = do
    Printer.pretty Chess.startPosition
    putStrLn "Enter move (e.g. e2-e4) >"
    line <- readLn
    let newPos = parseMove line Chess.startPosition
    Printer.pretty newPos
    return ()


parseMove :: String -> Position -> Position
parseMove s pos
    | s =~ "[a-h][1-8].[a-h][1-8]" = Chess.movePiece pos (parseFrom s) (parseTo s)
    | otherwise = pos

parseFrom x = (x !! 0, digitToInt (x !! 1))
parseTo x = (x !! 3, digitToInt (x !! 4))
