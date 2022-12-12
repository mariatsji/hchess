module Tester where

import qualified AI
import Chess
import Move
import Position
import Printer
import Evaluation
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)

test :: Int -> IO ()
test depth = do
    let perspective = Black
        -- Right pos' = playMoves ["e2-e4", "f7-f5", "d1-g4"]
        snp = m emptyBoard
        snp2 = replacePieceAt snp (Square 5 1) (King White)
        snp3 = replacePieceAt snp2 (Square 5 8) (King Black)
        snp4 = replacePieceAt snp3 (Square 4 2) (Queen White)
        snp5 = replacePieceAt snp4 (Square 5 5) (Knight Black)
        pos = emptyBoard { m = snp5, toPlay = Black, castleStatusWhite = CanCastleNone, castleStatusBlack = CanCastleNone, gamehistory = [snp4, snp3, snp2, snp2, snp] }
    pretty pos
    print $
        deepEval depth Black pos

testAI :: Int -> IO ()
testAI depth = do
    let perspective = Black
        -- Right pos' = playMoves ["e2-e4", "f7-f5", "d1-g4"]
        snp = m emptyBoard
        snp2 = replacePieceAt snp (Square 5 1) (King White)
        snp3 = replacePieceAt snp2 (Square 5 8) (King Black)
        snp4 = replacePieceAt snp3 (Square 4 2) (Queen White)
        snp5 = replacePieceAt snp4 (Square 5 5) (Knight Black)
        pos = emptyBoard { m = snp5, toPlay = Black, castleStatusWhite = CanCastleNone, castleStatusBlack = CanCastleNone, gamehistory = [snp4, snp3, snp2, snp2, snp] }
    traverse_
        pretty $
        AI.edgeGreed pos depth