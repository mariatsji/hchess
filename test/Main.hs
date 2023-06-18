module Main where

import qualified AISpec
import qualified CacheSpec
import qualified MoveSpec
import qualified PGNSpec
import Test.Hspec
import Relude

main :: IO ()
main = hspec $ do
    AISpec.spec
    CacheSpec.spec
    MoveSpec.spec
    PGNSpec.spec
