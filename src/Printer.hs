module Printer
  ( pretty,
    prettyE,
    prettyEs
    )
where

import Chess
import qualified Data.ByteString.Char8 as UP
import qualified Data.ByteString.UTF8 as UF
import qualified Data.IntMap.Strict as Map
import Evaluation
import Position (hash, unHash)
import GHC.Exts

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
rowify (Position m' _) = reverse $ groupWith (\(Square _ r, _) -> r) (listWithEmpties m')

listWithEmpties :: Map.IntMap Piece -> [(Square, Maybe Piece)]
listWithEmpties m' = fmap (\s -> (unHash s, Map.lookup s m')) (hash <$> board)

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
