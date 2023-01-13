module Main where

import AppContext (AppContext (..))
import Control.Monad.Trans.Reader (runReaderT)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as AT
import Data.Either (partitionEithers)
import Data.Text (pack, unpack)
import qualified Data.Text.IO as TIO
import GameLoop
import PGN (parsePgn)
import Position (Color (..), Position)
import Printer
import Style (Style, brightTheme, darkTheme)
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
    ctx <- mkContext args
    flip runReaderT ctx $ do
        l <- Printer.line
        start l

mkContext :: [String] -> IO AppContext
mkContext params = do
    pos <- findPGN params
    style <- findStyle params
    pure
        AppContext
            { analysis = "analysis" `elem` params
            , perspective = if "black" `elem` params then Black else White
            , whiteDepth = findWhite params
            , blackDepth = findBlack params
            , startFrom = pos
            , style = style
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

findStyle :: [String] -> IO Style
findStyle params = pure if "bright" `elem` params then brightTheme else darkTheme

findPGN :: [String] -> IO (Maybe Position)
findPGN params = do
    let (_, found) = partitionEithers $ parseOnly pgnLocation . pack <$> params
    case found of
        [] -> pure Nothing
        (filepath : _) -> do
            content <- TIO.readFile filepath
            case parsePgn content of
                Right pos -> pure $ Just pos
                _ -> pure Nothing

pgnLocation :: Parser FilePath
pgnLocation = do
    _ <- AT.string "pgn="
    loc <- AT.takeTill (== '.')
    _ <- AT.string ".pgn"
    pure $ unpack loc <> ".pgn"
