module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.Widget (widgetSetSizeRequest)
import qualified GI.Gio.Objects.Application as Gio
import Position
import System.IO.Unsafe (unsafePerformIO)

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

    {--vbox <- Gtk.boxNew Gtk.OrientationVertical 10
    Gtk.setWidgetMargin vbox 10
    Gtk.containerAdd window vbox
    Gtk.widgetShow vbox --}
    pic <- whitePawn (Square 1 1, Nothing)
    Gtk.containerAdd window pic 
    Gtk.widgetShow pic
    
    Gtk.widgetShow window



whitePawn :: (Square, Maybe Piece) -> IO Gtk.Image
whitePawn (Square c r, mPiece)= do
    area <- Gtk.drawingAreaNew
    let width = 60
        height = 60
    _ <- Gtk.widgetSetSizeRequest area width height
    --let squareCol = if even $ c + r then _brown else _white
    
    Gtk.imageNewFromFile "img/PawnWhite.png"



{--
Gtk.onButtonClicked button $ do
    Gtk.widgetSetSensitive button False
    _ <- forkIO $ do
        c <- getWeather
        _ <- GLib.idleAdd GLib.PRIORITY_HIGH_IDLE $ do
            _ <- Gtk.entrySetText entryC (renderDouble c)
            Gtk.widgetSetSensitive button True
            pure False
        pure ()
    pure ()      
--}


{--
-- input celcius
    entryC <- addEntry "° C" vbox
    -- input fahr
    entryF <- addEntry "° F" vbox
    -- processing logic
    _ <- Gtk.onEditableChanged entryC $ do
        s <- Gtk.entryGetText entryC
        Gtk.entrySetText entryF (Text.reverse s)
    
    -- button
    button <- Gtk.buttonNew
    Gtk.setButtonLabel button "Get Weather"
    Gtk.setWidgetHalign button Gtk.AlignCenter
    Gtk.containerAdd vbox button
    Gtk.widgetShow button
--}
addEntry :: Gtk.IsContainer a => Text -> a -> IO Gtk.Entry
addEntry labelStr container = do
    hbox <- Gtk.boxNew Gtk.OrientationHorizontal 5
    entry <- Gtk.entryNew
    label <- Gtk.labelNew $ Just labelStr
    Gtk.containerAdd hbox entry
    Gtk.containerAdd hbox label
    Gtk.containerAdd container hbox
    Gtk.setWidgetExpand entry True
    Gtk.setEntryXalign entry 1
    Gtk.widgetShow entry
    Gtk.widgetShow label
    Gtk.widgetShow hbox
    pure entry