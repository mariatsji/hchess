module AppContext where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import Position (Position)

data AppContext = AppContext
    { analysis :: Bool
    }

type App a = ReaderT AppContext IO a

data World = World
    { wTitle :: Text
    , wPos :: Maybe Position
    , wScore :: Maybe Float
    , wInfo :: [Text]
    }