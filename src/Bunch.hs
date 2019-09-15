{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This is where we group (Square, Piece)
module Bunch where

import Control.DeepSeq
import Data.Coerce
import GHC.Generics

newtype Bunch a = Bunch [a] deriving (Eq, Show, Generic, NFData, Functor, Applicative, Monoid, Foldable, Monad)

instance Semigroup (Bunch a) where
  (Bunch a) <> (Bunch b) = Bunch (a <> b)

unBunch :: Bunch a -> [a]
unBunch = coerce
{-# INLINE unBunch #-}

emptyBunch :: Bunch a
emptyBunch = Bunch []
{-# INLINE emptyBunch #-}

singleton :: a -> Bunch a
singleton x = Bunch [x]
{-# INLINE singleton #-}

filter' :: (a -> Bool) -> Bunch a -> Bunch a
filter' pred' bunch = {-# SCC "Bunch.filter'" #-} coerce $ filter pred' (unBunch bunch)
{-# INLINE filter' #-}

unsafeHead :: Bunch a -> a
unsafeHead = head . unBunch
{-# INLINE unsafeHead #-}
