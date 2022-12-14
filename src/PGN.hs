module PGN where

import NeatInterpolation

import Board (searchIdx)
import Chess (Status (..), determineStatus, isCheckMate, isInCheck, pieceAt, playIfLegal, positionTree)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Move (playMoves)
import Position (CastleStatus (CanCastleBoth), Color (..), Move (..), Piece (..), Position (..), Snapshot, Square (..), colr, findMove, next, pieceAt', startPosition)

renderPgn :: Position -> Text
renderPgn pos' =
    let res = renderResult pos'
        mo = renderMoves pos'
     in [text|
        [Event "hChess match"]
        [Site "In front of computer"]
        [Date "2022-12-13"]
        [Round "1"]
        [White "Humanoid Contender"]
        [Black "Computer"]
        [Result = "$res"]

        $mo $res
    |]

renderMoves :: Position -> Text
renderMoves Position {..} =
    let indices = repeatEntries [1 ..]
        snapshots = reverse gamehistory `zip` indices
     in go snapshots
  where
    go [] = ""
    go [(from, i)] = renderNr i i
    go ((from, i) : (to, j) : more) = renderNr i j <> renderMove from to <> " " <> go ((to, j) : more)


renderNr :: Int -> Int -> Text
renderNr i j = if i == j then pack $ show i <> ". " else ""

repeatEntries :: [a] -> [a]
repeatEntries [] = []
repeatEntries (x : xs) = x : x : repeatEntries xs

renderMove :: Snapshot -> Snapshot -> Text
renderMove from to =
    let move' = findMove from to
     in case move' of
            CastleShort -> "O-O"
            CastleLong -> "O-O-O"
            MovedPiece fromSq toSq ->
                let fromPiece' = fromPiece from fromSq
                    fromColor' = maybe (error "PGN missing color in from piece") colr (pieceAt' from fromSq)
                 in renderPiece fromPiece' <> pack (show fromSq) <> renderTakes from to <> pack (show toSq) <> renderCheck to fromColor' -- todo duplication
            Promotion fromSq toSq piece ->
                let fromPiece' = fromPiece from fromSq
                    fromColor' = maybe (error "PGN missing color in from piece") colr (pieceAt' from fromSq)
                 in renderPiece fromPiece' <> pack (show fromSq) <> renderTakes from to <> pack (show toSq) <> renderProm piece <> renderCheck to fromColor'

renderCheck :: Snapshot -> Color -> Text
renderCheck snp mover =
    let fakePos =
            Position
                { m = snp
                , gamehistory = [m startPosition]
                , castleStatusWhite = CanCastleBoth
                , castleStatusBlack = CanCastleBoth
                , toPlay = next mover
                }
     in if isCheckMate fakePos (positionTree fakePos) then "#" else if isInCheck fakePos then "!" else ""

renderProm :: Piece -> Text
renderProm = (<>) "=" . renderPiece

renderTakes :: Snapshot -> Snapshot -> Text
renderTakes from to =
    if countPieces from White == countPieces to White && countPieces from Black == countPieces to Black then "" else "x"
  where
    countPieces :: Snapshot -> Color -> Int
    countPieces s c = length $ searchIdx s (const True) (\mp -> fmap colr mp == Just c)

renderPiece :: Piece -> Text
renderPiece = \case
    Pawn _ -> ""
    Knight _ -> "N"
    Bishop _ -> "B"
    Rook _ -> "R"
    Queen _ -> "Q"
    King _ -> "K"

fromPiece :: Snapshot -> Square -> Piece
fromPiece snp s = fromMaybe (error "PGN found no fromPiece in snapshot") (pieceAt' snp s)

renderResult :: Position -> Text
renderResult pos = case determineStatus pos of
    WhiteIsMate -> "0-1"
    BlackIsMate -> "1-0"
    Remis -> "1/2-1/2"
    _ -> "*"

pgnTester :: IO ()
pgnTester = do
    let Right testPos = playMoves ["e2-e3", "f7-f6", "f2-f4", "g7-g5", "d1-h5"]
    print $ renderPgn testPos