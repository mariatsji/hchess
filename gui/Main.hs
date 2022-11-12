module Main where

import Data.Text (Text)
import qualified GI.Gtk as Gtk
import qualified GI.Gio.Objects.Application as Gio
import Position

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
    Gtk.widgetShowAll window