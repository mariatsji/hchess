{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

{--
import Position
import Test.QuickCheck

main = quickCheck prop_hash

prop_hash :: [Int] -> Bool
prop_hash xs = (hash <$> (unHash <$> xs)) == xs
--}