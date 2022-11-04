module CacheSpec where

import Cache
import Test.Hspec
import Position (startPosition)
import Evaluation (evaluate')
import Chess (positionTree)

spec :: Spec
spec = describe "Cache" $ do
  it "inserts some evaluated and fetches them back out" $ do
    let pos = startPosition
        key = mkKey startPosition
        evs = evaluate' <$> positionTree pos
        cache = insert pos evs fresh
        foundEvs = lookup' pos cache 
    length evs `shouldBe` length foundEvs
