module Move(parseMove, parseMoves) where

import Chess
import Data.Char
import Text.Regex.TDFA

parseMove :: String -> GameHistory -> GameHistory
parseMove s gh
    | s =~ "[a-h][1-8].[a-h][1-8]Q" =
            let moveAttempt = promoteTo (toPlay gh) (Chess.movePiece (head gh) (parseFrom s) (parseTo s)) (Queen White)
                legalMoves = Chess.positionTree gh
                colorToPlay = if whiteToPlay gh then Just White else Just Black
                fromSquare = parseFrom s
            in if moveAttempt `elem` legalMoves && colorToPlay == fmap color (pieceAt (head gh) fromSquare) then moveAttempt : gh else gh
    | s =~ "[a-h][1-8].[a-h][1-8]R" =
            let moveAttempt = promoteTo (toPlay gh) (Chess.movePiece (head gh) (parseFrom s) (parseTo s)) (Rook White)
                legalMoves = Chess.positionTree gh
                colorToPlay = if whiteToPlay gh then Just White else Just Black
                fromSquare = parseFrom s
            in if moveAttempt `elem` legalMoves && colorToPlay == fmap color (pieceAt (head gh) fromSquare) then moveAttempt : gh else gh
    | s =~ "[a-h][1-8].[a-h][1-8]B" =
            let moveAttempt = promoteTo (toPlay gh) (Chess.movePiece (head gh) (parseFrom s) (parseTo s)) (Bishop White)
                legalMoves = Chess.positionTree gh
                colorToPlay = if whiteToPlay gh then Just White else Just Black
                fromSquare = parseFrom s
            in if moveAttempt `elem` legalMoves && colorToPlay == fmap color (pieceAt (head gh) fromSquare) then moveAttempt : gh else gh
    | s =~ "[a-h][1-8].[a-h][1-8]K" =
            let moveAttempt = promoteTo (toPlay gh) (Chess.movePiece (head gh) (parseFrom s) (parseTo s)) (Knight White)
                legalMoves = Chess.positionTree gh
                colorToPlay = if whiteToPlay gh then Just White else Just Black
                fromSquare = parseFrom s
            in if moveAttempt `elem` legalMoves && colorToPlay == fmap color (pieceAt (head gh) fromSquare) then moveAttempt : gh else gh
    | s =~ "[a-h][1-8].[a-h][1-8]" =
            let moveAttempt = Chess.movePiece (head gh) (parseFrom s) (parseTo s)
                legalMoves = Chess.positionTree gh
                colorToPlay = if whiteToPlay gh then Just White else Just Black
                fromSquare = parseFrom s
            in if any (eqPosition moveAttempt) legalMoves && colorToPlay == fmap color (pieceAt (head gh) fromSquare) then moveAttempt : gh else gh
    | s =~ "O-O" =
            let castleAttempt = Chess.castleShort gh (toPlay gh)
                legalMoves = Chess.positionTree gh
                colorToPlay = if whiteToPlay gh then Just White else Just Black
                fromSquare = if (toPlay gh) == White then ('e', 1) else ('e', 8)
            in if [] /= castleAttempt && (head castleAttempt) `elem` legalMoves && colorToPlay == fmap color (pieceAt (head gh) fromSquare) then (head castleAttempt) : gh else gh
    | s =~ "O-O-O" =
            let castleAttempt = Chess.castleLong gh (toPlay gh)
                legalMoves = Chess.positionTree gh
                colorToPlay = if whiteToPlay gh then Just White else Just Black
                fromSquare = if (toPlay gh) == White then ('e', 1) else ('e', 8)
            in if [] /= castleAttempt && (head castleAttempt) `elem` legalMoves && colorToPlay == fmap color (pieceAt (head gh) fromSquare) then (head castleAttempt) : gh else gh
    | otherwise = gh

parseMoves :: [String] -> GameHistory
parseMoves moves = foldl (flip parseMove) [Chess.startPosition] moves

parseFrom x = (head x, digitToInt (x !! 1))
parseTo x = (x !! 3, digitToInt (x !! 4))

