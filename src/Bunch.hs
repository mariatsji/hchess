{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This is where we group (Square, Piece)
module Bunch where

import Control.DeepSeq
import GHC.Generics

newtype Bunch a = Bunch [a] deriving (Eq, Show, Generic, NFData, Functor, Applicative, Foldable, Monad)

unBunch :: Bunch a -> [a]
unBunch (Bunch a) = a
{-# INLINE unBunch #-}