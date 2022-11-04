module Cache (Cache, fresh, mkKey, insert, lookup') where

import Data.Coerce (coerce)
import Data.Multimap (Multimap)
import qualified Data.Multimap as M
import Evaluation
import Position (Color, Position (Position, m, toPlay), Snapshot, hash)

type Key = (Snapshot, Color)

newtype Cache = Cache {unCache :: Multimap Key Evaluated}

fresh :: Cache
fresh = Cache M.empty

maxCacheSize :: Int
maxCacheSize = 1_000_000

mkKey :: Position -> Key
mkKey pos =
    let snp = m pos
     in (snp, toPlay pos)

-- give back the same Cache if the cache is full
insert :: Position -> [Evaluated] -> Cache -> Cache
insert pos vals cache =
    let key = mkKey pos
        theMap :: Multimap Key Evaluated = coerce cache
     in if M.size theMap < maxCacheSize
            then multiInsert key vals cache
            else cache

multiInsert :: Key -> [Evaluated] -> Cache -> Cache
multiInsert k [] c = c
multiInsert k (e : es) c =
    coerce $
        M.insert k e $
            coerce (multiInsert k es c)

lookup' :: Position -> Cache -> [Evaluated]
lookup' pos cache =
    let key = mkKey pos
        theMap :: Multimap Key Evaluated = coerce cache
     in M.lookup key theMap