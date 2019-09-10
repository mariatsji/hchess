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

filter' :: (a -> Bool) -> Bunch a -> Bunch a
filter' pred' bunch = coerce $ filter pred' (unBunch bunch)