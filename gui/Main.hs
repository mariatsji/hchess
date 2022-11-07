module Main where

import qualified GUI
import Position ( startPosition )
import Graphics.UI.Gtk

type World = (Position, Mabybe Square) -- store the world and also a potential fromSquare (after user clicks it)

-- https://hackage.haskell.org/package/gi-gtk-declarative-app-simple-0.7.1/docs/GI-Gtk-Declarative-App-Simple.html
main :: IO ()
main = do
    let initialState = (newPosition, Nothing)
    let app = App
        _update
        _view
        _inputs
        initialState
    newState <- run app