module Main where

import AppContext (AppContext (..))
import Control.Monad.Trans.Reader (runReaderT)
import GameLoop
import Printer
import System.Console.ANSI
import System.Environment (getArgs)

main :: IO ()
main = do
    clearScreen
    setCursorPosition 0 0
    setTitle "hChess"
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
        }