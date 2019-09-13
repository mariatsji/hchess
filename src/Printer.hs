module Printer
where

import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF
import Evaluation
import GHC.Exts
import Position

pretty :: Position -> IO ()
pretty pos = do
  mapM_ UP.putStrLn $ prettyRow <$> rowify pos
  putStrLn "-"
  return ()

prettyE :: Evaluated -> IO ()
prettyE (Evaluated gh score status) = do
  pretty gh
  putStrLn $ "score : " ++ show score
  putStrLn $ "status : " ++ show status

prettyEs :: [Evaluated] -> IO ()
prettyEs = mapM_ prettyE

rowify :: Position -> [[(Square, Maybe Piece)]]
rowify pos = reverse $ groupWith (\(Square _ r, _) -> r) (asListWithEmpties' (m pos))

prettyRow :: [(Square, Maybe Piece)] -> UF.ByteString
prettyRow row =
  UF.fromString $ foldl1 (\a s -> a ++ " " ++ s) $ fmap prettyPiece row

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
