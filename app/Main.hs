module Main where

import AppContext (AppContext (..))
import Control.Monad.Trans.Reader (runReaderT)
import GameLoop
import Position (Color (..))
import Printer
import qualified System.Console.ANSI as ANSI
import System.Environment (getArgs)

main :: IO ()
main = do
    ANSI.clearScreen
    ANSI.setCursorPosition 0 0
    ANSI.setTitle "hChess"
    putStrLn "1 Human vs Human"
    putStrLn "2 Human vs Machine"
    putStrLn "3 Machine vs Machine"
    putStrLn "q Quit"
    args <- getArgs
    let ctx = mkContext args
    flip runReaderT ctx $ do
        l <- Printer.line
        start l

mkContext :: [String] -> AppContext
mkContext params =
    AppContext
        { analysis = "analysis" `elem` params
        , perspective = if "black" `elem` params then Black else White
        , whiteDepth = findWhite params
        , blackDepth = findBlack params
        }

findWhite :: [String] -> Int
findWhite params
  | "w0" `elem` params = 0
  | "w1" `elem` params = 1
  | "w3" `elem` params = 3
  | otherwise = 2

findBlack :: [String] -> Int
findBlack params
  | "b0" `elem` params = 0
  | "b1" `elem` params = 1
  | "b3" `elem` params = 3
  | otherwise = 2