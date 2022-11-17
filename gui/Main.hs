module Main where

import Data.Text (Text)
import GI.GdkPixbuf.Enums (InterpType (InterpTypeBilinear))
import qualified GI.GdkPixbuf.Objects.Pixbuf as Pixbuf
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.Fixed as Gtk.Fixed
import qualified GI.Gtk.Objects.GestureClick as GestureClick
import qualified GI.Gtk.Objects.Widget as Widget
import qualified GI.Gtk.Objects.Window as Window

import Chess (board)
import Data.Foldable (traverse_)
import Data.Int (Int32)
import Data.Maybe (listToMaybe)
import qualified GI.Gio.Objects.Application as Gio
import Position

type World = (Position, Maybe Square) -- store the world and also a potential fromSquare (after user clicks it)

main :: IO ()
main = do
    let world = (startPosition, Just (Square 5 2))
    app <- Gtk.applicationNew (Just appId) []
    Gio.onApplicationActivate app (appActivate app world)
    Gio.applicationRun app Nothing
    pure ()

appId :: Text
appId = "io.grevling.hchess"

sizeScale :: Int32
sizeScale = 100

appActivate :: Gtk.Application -> World -> IO ()
appActivate app world = do
    window <- Gtk.applicationWindowNew app
    Gtk.setWindowTitle window "hChess"
    Gtk.setWindowResizable window True
    Gtk.setWindowDefaultWidth window 1000
    Gtk.setWindowDefaultHeight window 1000

    fixedArea <- Gtk.Fixed.fixedNew

    Window.windowSetChild window (Just fixedArea)

    -- add event listener to drags
    clickEvent <- GestureClick.gestureClickNew
    GestureClick.onGestureClickPressed clickEvent clickBegin
    GestureClick.onGestureClickReleased clickEvent clickEnd
    Widget.widgetAddController
        fixedArea
        clickEvent

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
    (Just buf) <-
        Pixbuf.pixbufNewFromFile $
            if mHighlight == Just sq
                then "img/highlight.square.png"
                else
                    if even (c + r)
                        then "img/dark.square.png"
                        else "img/bright.square.png"

    bufScaled <- Pixbuf.pixbufScaleSimple buf sizeScale sizeScale InterpTypeBilinear

    finalSquareImage <-
        Gtk.imageNewFromPixbuf bufScaled
    size <- Gtk.getImagePixelSize finalSquareImage
    Gtk.imageSetPixelSize finalSquareImage sizeScale

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
            (Just pieceBuf) <- loadPieceImage piece
            bufScaled <- Pixbuf.pixbufScaleSimple pieceBuf sizeScale sizeScale InterpTypeBilinear
            pieceImage <- Gtk.imageNewFromPixbuf bufScaled
            Gtk.imageSetPixelSize pieceImage sizeScale
            Gtk.Fixed.fixedPut
                fixed
                pieceImage
                (xCoord sq)
                (yCoord sq)
        )
        mPiece

clickBegin :: Int32 -> Double -> Double -> IO ()
clickBegin nrClicks x y = do
    print $ "its a drag from [" <> show (findSquare x y, (x,y)) <> "]"

clickEnd :: Int32 -> Double -> Double -> IO ()
clickEnd nrClicks x y = print $ "its a drop at [" <> show (findSquare x y, (x,y)) <> "]"

findSquare :: Double -> Double -> Maybe Square
findSquare x y = listToMaybe [square | square <- board, xCoord square `closeTo` x && yCoord square `closeTo` y]
  where
    closeTo :: Double -> Double -> Bool
    closeTo a b = abs (a - b) < 15

-- todo buffer this plz
loadPieceImage :: Piece -> IO (Maybe Pixbuf.Pixbuf)
loadPieceImage piece = do
    let filename = case piece of
            Pawn White -> "img/PawnWhite.png"
            Pawn Black -> "img/PawnBlack.png"
            Knight White -> "img/KnightWhite.png"
            Knight Black -> "img/KnightBlack.png"
            Bishop White -> "img/BishopWhite.png"
            Bishop Black -> "img/BishopBlack.png"
            Rook White -> "img/RookWhite.png"
            Rook Black -> "img/RookBlack.png"
            Queen White -> "img/QueenWhite.png"
            Queen Black -> "img/QueenBlack.png"
            King White -> "img/KingWhite.png"
            King Black -> "img/KingBlack.png"
    Pixbuf.pixbufNewFromFile filename

-- ---> x (more x is more to the right)
xCoord :: Square -> Double
xCoord (Square c _) = fromIntegral sizeScale * ([0 .. 8] !! c)

{--
  |
  |
  | y (more y is more down)
--}
yCoord :: Square -> Double
yCoord (Square _ r) = let marginTop = 80 in (fromIntegral sizeScale * (reverse [0 .. 8] !! r)) + marginTop
