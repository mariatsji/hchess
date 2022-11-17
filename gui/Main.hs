module Main where

import Data.Text (Text)
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.Fixed as Gtk.Fixed
import qualified GI.Gtk.Objects.Widget as Widget
import qualified GI.Gtk.Objects.GestureClick as GestureClick

import Data.Foldable (traverse_)
import Data.Int (Int32)
import qualified GI.Gio.Objects.Application as Gio
import Position

type World = (Position, Maybe Square) -- store the world and also a potential fromSquare (after user clicks it)

main :: IO ()
main = do
    let world = (startPosition, Just (Square 5 2))
    Just app <- Gtk.applicationNew (Just appId) []
    Gio.onApplicationActivate app (appActivate app world)
    Gio.applicationRun app Nothing
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

    -- add event listener to drags
    clickEvent <- GestureClick.gestureClickNew
    GestureClick.onGestureClickPressed clickEvent clickBegin
    GestureClick.onGestureClickReleased clickEvent clickEnd
    Widget.widgetAddController
        fixedArea
        click

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

clickBegin :: Double -> Double -> IO ()
clickBegin x y = print $ "its a drag [" <> show (x, y) <> "]"

clickEnd :: Double -> Double -> IO ()
clickEnd x y = print $ "its a drop [" <> show (x, y) <> "]"

-- todo buffer this plz
loadPieceImage :: Piece -> IO Gtk.Image
loadPieceImage =
    Gtk.imageNewFromFile . \case
        Pawn White -> "img/PawnWhite6060.png"
        Pawn Black -> "img/PawnBlack6060.png"
        Knight White -> "img/KnightWhite6060.png"
        Knight Black -> "img/KnightBlack6060.png"
        Bishop White -> "img/BishopWhite6060.png"
        Bishop Black -> "img/BishopBlack6060.png"
        Rook White -> "img/RookWhite6060.png"
        Rook Black -> "img/RookBlack6060.png"
        Queen White -> "img/QueenWhite6060.png"
        Queen Black -> "img/QueenBlack6060.png"
        King White -> "img/KingWhite6060.png"
        King Black -> "img/KingBlack6060.png"

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