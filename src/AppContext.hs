module AppContext where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Position (Color, Position)

data AppContext = AppContext
    { analysis :: Bool
    , perspective :: Color
    }

type App a = ReaderT AppContext IO a

data World = World
    { wTitle :: Text
    , wPos :: Maybe Position
    , wScore :: Maybe Float
    , wInfo :: [Text]
    }