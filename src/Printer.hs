module Printer
  ( pretty
  , prettyGH
  , prettyGH'
  , prettyE
  , prettyEs
  ) where

import           Chess
import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8  as UF
import           Evaluation
import           GHC.Exts

pretty :: Position -> IO ()
pretty pos = do
  UP.putStrLn (prettify pos)
  putStrLn "-"
  return ()

prettyE :: Evaluated -> IO ()
prettyE (Evaluated gh score status) = do
  prettyGH' gh
  putStrLn $ "score : " ++ show score
  putStrLn $ "status : " ++ show status

prettyEs :: [Evaluated] -> IO ()
prettyEs = mapM_ prettyE

prettyGH :: GameHistory -> IO ()
prettyGH gh = mapM_ pretty (reverse gh)

prettyGH' :: GameHistory -> IO ()
prettyGH' = pretty . head

boardProjection :: [Square]
boardProjection = Square <$> reverse [1 .. 8] <*> [1 ..8] 

rowRepresentation :: [a] -> [[a]]
rowRepresentation []   = []
rowRepresentation rows = take 8 rows : rowRepresentation (drop 8 rows)

prettify :: Position -> UF.ByteString
prettify pos =
  let squares = fmap (\s -> prettyPiece (s, pieceAt pos s)) boardProjection
      rows = rowRepresentation squares
      board = unlines $ fmap unwords rows
  in UF.fromString board

prettyPiece :: (Square, Maybe Piece) -> String
prettyPiece (Square c r, Nothing) =
  if even (c + r)
    then "▯"
    else " "
prettyPiece (_, Just (Pawn White)) = "♙"
prettyPiece (_, Just (Knight White)) = "♘"
prettyPiece (_, Just (Bishop White)) = "♗"
prettyPiece (_, Just (Rook White)) = "♖"
prettyPiece (_, Just (Queen White)) = "♕"
prettyPiece (_, Just (King White)) = "♔"
prettyPiece (_, Just (Pawn Black)) = "♟"
prettyPiece (_, Just (Knight Black)) = "♞"
prettyPiece (_, Just (Bishop Black)) = "♝"
prettyPiece (_, Just (Rook Black)) = "♜"
prettyPiece (_, Just (Queen Black)) = "♛"
prettyPiece (_, Just (King Black)) = "♚"
