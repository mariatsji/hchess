module Main where

import Data.Text (Text)
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.Fixed as Gtk.Fixed

import Data.Foldable (traverse_)
import Data.Int (Int32)
import qualified GI.Gio.Objects.Application as Gio
import Position

type World = (Position, Maybe Square) -- store the world and also a potential fromSquare (after user clicks it)

main :: IO ()
main = do
    let world = (startPosition, Just (Square 5 2))
    Just app <- Gtk.applicationNew (Just appId) []
    _ <- Gio.onApplicationActivate app (appActivate app world)
    _ <- Gio.applicationRun app Nothing
    pure ()

appId :: Text
appId = "io.grevling.hchess"

appActivate :: Gtk.Application -> World -> IO ()
appActivate app world = do
    window <- Gtk.applicationWindowNew app
    Gtk.setWindowTitle window "hChess"
    Gtk.setWindowResizable window True
    Gtk.setWindowDefaultWidth window 1000
    Gtk.setWindowDefaultHeight window 1000

    fixedArea <- Gtk.Fixed.fixedNew
    Gtk.containerAdd window fixedArea

    drawWorld fixedArea world
    Gtk.widgetShow fixedArea
    Gtk.widgetShow window

drawWorld :: Gtk.Fixed.Fixed -> World -> IO ()
drawWorld fixedArea (pos, mHighlight) =
    traverse_
        (drawSquare fixedArea mHighlight)
        (toList' (m pos))

drawSquare :: Gtk.Fixed.Fixed -> Maybe Square -> (Square, Maybe Piece) -> IO ()
drawSquare fixed mHighlight (sq@(Square c r), mPiece) = do
    squareImg <-
        Gtk.imageNewFromFile
            if even (c + r)
                then "img/dark.square.png"
                else "img/bright.square.png"

    highlightImg <- Gtk.imageNewFromFile "img/highlight.square.png"

    let finalSquareImage =
            if mHighlight == Just sq
                then highlightImg
                else squareImg

    Gtk.widgetShow finalSquareImage

    -- draw square behind the piece
    Gtk.Fixed.fixedPut
        fixed
        finalSquareImage
        (xCoord sq)
        (yCoord sq)

    -- draw piece on top maybe
    maybe
        (pure ())
        ( \piece -> do
            pieceImage <- loadPieceImage piece
            Gtk.Fixed.fixedPut
                fixed
                pieceImage
                (xCoord sq)
                (yCoord sq)

            Gtk.widgetShow pieceImage
        )
        mPiece

-- let squareCol = if even $ c + r then _brown else _white

-- todo buffer this plz
loadPieceImage :: Piece -> IO Gtk.Image
loadPieceImage = \case
    Pawn White -> Gtk.imageNewFromFile "img/PawnWhite6060.png"
    Pawn Black -> Gtk.imageNewFromFile "img/PawnBlack6060.png"
    Knight White -> Gtk.imageNewFromFile "img/KnightWhite6060.png"
    Knight Black -> Gtk.imageNewFromFile "img/KnightBlack6060.png"
    Bishop White -> Gtk.imageNewFromFile "img/BishopWhite6060.png"
    Bishop Black -> Gtk.imageNewFromFile "img/BishopBlack6060.png"
    Rook White -> Gtk.imageNewFromFile "img/RookWhite6060.png"
    Rook Black -> Gtk.imageNewFromFile "img/RookBlack6060.png"
    Queen White -> Gtk.imageNewFromFile "img/QueenWhite6060.png"
    Queen Black -> Gtk.imageNewFromFile "img/QueenBlack6060.png"
    King White -> Gtk.imageNewFromFile "img/KingWhite6060.png"
    King Black -> Gtk.imageNewFromFile "img/KingBlack6060.png"

-- ---> x (more x is more to the right)
xCoord :: Square -> Int32
xCoord (Square c _) = 60 * ([0 .. 8] !! c)

{--
  |
  |
  | y (more y is more down)
--}
yCoord :: Square -> Int32
yCoord (Square _ r) = let marginTop = 80 in (60 * (reverse [0 .. 8] !! r)) + marginTop