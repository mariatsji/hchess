module AppContext where

import Position (Color, Position)
import Style (Style)

import Relude

data AppContext = AppContext
    { analysis :: Bool -- print scores
    , perspective :: Color
    , whiteDepth :: Int
    , blackDepth :: Int
    , startFrom :: Maybe Position -- any previous moves to play off from 
    , style :: Style
    }

type App a = ReaderT AppContext IO a

data World = World
    { wTitle :: Text
    , wPos :: Maybe Position
    , wScore :: Maybe Float
    , wInfo :: [Text]
    }