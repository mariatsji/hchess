module PGN where

import NeatInterpolation

import Position (Move)
import Data.Text (Text)

type Title = Text

data Result = W | B | R

data PgnMove = Todo

data Pgn = Pgn {
    title :: Title,
    result :: Result,
    moves :: [PgnMove]
}

convert :: Move -> PgnMove
convert = undefined

pgn :: Move -> Pgn -> Pgn
pgn move prev =
    let existing = moves prev
    in prev { moves = existing <> [convert move] }

renderPgn :: Pgn -> Text
renderPgn Pgn{..} =
    let res = renderResult result
        mo = renderMoves moves
    in
    [text|
        [Event "$title"]
        [Site "In front of computer"]
        [Date "2022-12-13"]
        [Round "1"]
        [White "Humanoid Contender"]
        [Black "Computer"]
        [Result = "$res"]

        $mo $res
    |]

renderMoves :: [PgnMove] -> Text
renderMoves _ = "" -- todo, moves 

renderResult :: Result -> Text
renderResult W = "1-0"
renderResult B = "0-1"
renderREsult R = "1/2-1/2"