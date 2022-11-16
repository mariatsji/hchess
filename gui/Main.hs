module Main where

import Data.Text (Text)
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.Fixed as Gtk.Fixed
import qualified GI.GdkPixbuf as Pixbuf

import qualified GI.Gio.Objects.Application as Gio
import Position
import Data.Foldable (traverse_)
import Data.Int (Int32)

type World = (Position, Maybe Square) -- store the world and also a potential fromSquare (after user clicks it)

main :: IO ()
main = do
    Just app <- Gtk.applicationNew (Just appId) []
    _ <- Gio.onApplicationActivate app (appActivate app)
    _ <- Gio.applicationRun app Nothing
    pure ()

appId :: Text
appId = "io.grevling.hchess"

appActivate :: Gtk.Application -> IO ()
appActivate app = do
    window <- Gtk.applicationWindowNew app
    Gtk.setWindowTitle window "hChess"
    Gtk.setWindowResizable window True
    Gtk.setWindowDefaultWidth window 1000
    Gtk.setWindowDefaultHeight window 1000

    fixedArea <- Gtk.Fixed.fixedNew
    Gtk.containerAdd window fixedArea

    traverse_
        (drawSquare fixedArea)
        (toList' (m startPosition))

    Gtk.widgetShow fixedArea
    Gtk.widgetShow window


drawSquare :: Gtk.Fixed.Fixed -> (Square, Maybe Piece) -> IO ()
drawSquare fixed (sq, mPiece)= do

    img <- case mPiece of
        Just (Pawn White) -> Gtk.imageNewFromFile "img/PawnWhite.png"
        Just (Pawn Black) -> Gtk.imageNewFromFile "img/PawnBlack.png"
        Just (Knight White) -> Gtk.imageNewFromFile "img/KnightWhite.png"
        Just (Knight Black) -> Gtk.imageNewFromFile "img/KnightBlack.png"
        Just (Bishop White) -> Gtk.imageNewFromFile "img/BishopWhite.png"
        Just (Bishop Black) -> Gtk.imageNewFromFile "img/BishopBlack.png"
        Just (Rook White) -> Gtk.imageNewFromFile "img/RookWhite.png"
        Just (Rook Black) -> Gtk.imageNewFromFile "img/RookBlack.png"
        Just (Queen White) -> Gtk.imageNewFromFile "img/QueenWhite.png"
        Just (Queen Black) -> Gtk.imageNewFromFile "img/QueenBlack.png"
        Just (King White) -> Gtk.imageNewFromFile "img/KingWhite.png"
        Just (King Black) -> Gtk.imageNewFromFile "img/KingBlack.png"
        _ -> Gtk.imageNewFromFile "img/PawnWhite.png" -- todo wrong

    Gtk.setImagePixelSize img 30

    Gtk.Fixed.fixedPut
        fixed
        img
        (xCoord sq)
        (yCoord sq)
       
    --let squareCol = if even $ c + r then _brown else _white
    
    Gtk.widgetShow img

    
    
xCoord :: Square -> Int32
xCoord (Square c _) = 60 * ([0 ..] !! c)

yCoord :: Square -> Int32
yCoord (Square _ r) = 60 * ([0 ..] !! r)