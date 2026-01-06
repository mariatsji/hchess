module Main where

import qualified AISpec
import qualified MoveSpec
import qualified PGNSpec
import Relude
import Test.Hspec

main :: IO ()
main = hspec $ do
  AISpec.spec
  MoveSpec.spec
  PGNSpec.spec
