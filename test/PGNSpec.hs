module PGNSpec where

import Test.Hspec ( shouldBe, it, describe, Spec )
import PGN ( renderPgn, parsePgn )
import Position ( startPosition )

spec :: Spec
spec = do
  describe "PGN" $ do
    it "renders PGN of a position, and back into a position" $ do
      let pgn = renderPgn startPosition
      parsePgn pgn `shouldBe` Right startPosition