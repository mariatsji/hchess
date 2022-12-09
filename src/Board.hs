module Board (Board, set, (?!), empty64, searchIdx, diff) where

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IMap
import Data.Word (Word8)
import GHC.Generics (Generic)

type Board a = IMap.IntMap a

set :: Board a -> Word8 -> a -> Board a
set s w a = IMap.insert (fromIntegral w) a s

(?!) :: Board a -> Word8 -> a
(?!) s w = s IMap.! fromIntegral w

empty64 :: a -> Board a
empty64 a = IMap.fromList $ [0..63] `zip` repeat a

-- tune this
searchIdx :: NFData a => Board a -> (Word8 -> Bool) -> (a -> Bool) -> [(Word8, a)]
searchIdx s idxPred piecePred =
    [0 .. 63 :: Word8]
        >>= ( \i ->
                let mP = s IMap.! fromIntegral i
                 in [(i, mP) | idxPred i && piecePred mP]
            )

diff :: (NFData a, Eq a) => Board a -> Board a -> [(Word8, a)]
diff a b = searchIdx b (\w8 -> a ?! w8 /= b ?! w8) (const True)