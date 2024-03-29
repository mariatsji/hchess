module CacheSpec where

import Cache
import Position (startPosition)
import Evaluation (evaluate')
import Chess (positionTree)

import Test.Hspec
import Relude

spec :: Spec
spec = describe "Cache" $ do
  it "inserts some evaluated and fetches them back out" $ do
    let pos = startPosition
        evs = evaluate' <$> positionTree pos
        cache = insert pos evs fresh
        foundEvs = lookup' pos cache 
    length evs `shouldBe` length foundEvs
