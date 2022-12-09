module Board (Board, set, (?!), empty64, searchIdx, diff) where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import Data.Sequence (Seq, fromList, index, update)
import Data.Word (Word8)
import GHC.Generics (Generic)

data Board a = Board (Seq a)
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (NFData)

instance Functor Board where
    fmap f (Board x) = Board (f <$> x)

instance Foldable Board where
    foldr f b (Board a) = foldr f b a

set :: Board a -> Word8 -> a -> Board a
set (Board s) w a = Board (update (fromIntegral w) a s)

(?!) :: Board a -> Word8 -> a
(?!) (Board s) w = s `index` fromIntegral w

empty64 :: a -> Board a
empty64 a = Board $ fmap (const a) (fromList [0 .. 63])

-- tune this
searchIdx :: NFData a => Board a -> (Word8 -> Bool) -> (a -> Bool) -> [(Word8, a)]
searchIdx (Board s) idxPred piecePred =
    let indexed = [0 .. 63] `zip` toList s
     in filter (\(square, mP) -> idxPred square && piecePred mP) indexed

diff :: (NFData a, Eq a) => Board a -> Board a -> [(Word8, a)]
diff a b = searchIdx b (\w8 -> a ?! w8 /= b ?! w8) (const True)