module AppContext where

import Control.Monad.Trans.Reader (ReaderT)

data AppContext = AppContext {
    analysis :: Bool
    }

type App a = ReaderT AppContext IO a
