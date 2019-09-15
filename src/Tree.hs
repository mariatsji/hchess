{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Tree where

import Data.Bits
import Data.Word
import Control.DeepSeq
import GHC.Generics

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show, Generic, NFData)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x y) = Node (fmap f x) (fmap f y)

instance Applicative Tree where
  pure = Leaf
  (Leaf f) <*> (Leaf x) = Leaf (f x)
  (Leaf f) <*> (Node x y) = Node (f <$> x) (f <$> y)
  (Node t1 t2) <*> x = Node (t1 <*> x) (t2 <*> x)

instance Monad Tree where
  return = pure
  (Leaf a) >>= f = f a
  (Node a b) >>= f = Node (a >>= f) (b >>= f)

instance Semigroup (Tree a) where
  (<>) = Node

set :: Tree a -> Word8 -> a -> Tree a
set (Leaf _) _ y = Leaf y
set (Node l r) i y = {-# SCC "Tree.set" #-}
  let newI = shift i (-1)
   in if testBit i 0
        then
          let newR = set r newI y
           in Node l newR
        else
          let newL = set l newI y
           in Node newL r

(?!) :: Tree a -> Word8 -> a
(?!) (Leaf x) _ = x
(?!) (Node l r) i = {-# SCC "Tree.?" #-}
  if testBit i 0
    then r ?! shift i (-1)
    else l ?! shift i (-1)
{-# INLINE (?!) #-}

infixr 9 ?!

search :: Tree a -> (a -> Bool) -> [a]
search (Leaf x) pred' = [x | pred' x]
search (Node l r) pred' = {-# SCC "Tree.search" #-} search l pred' <> search r pred'
{-# INLINE search #-}

searchIdx :: Tree a -> (a -> Bool) -> [(Word8, a)]
searchIdx tree' pred' = go tree' pred' 0
  where go :: Tree a -> (a -> Bool) -> Word8 -> [(Word8, a)]
        go (Leaf x) p i = [(i, x) | p x]
        go (Node l r) p i = go l p (shift i 1) <> go r p (shift i 1 + 1)

empty64 :: a -> Tree a
empty64 = {-# SCC "Tree.empty64" #-} go 6
  where
    go :: Word8 -> a -> Tree a
    go 0 x = Leaf x
    go n x = Node (go (n - 1) x) (go (n - 1) x)
{-# INLINE empty64 #-}

fromList :: [a] -> Tree a -- 64 Leafs.. sorry mac, that's what you get - this is chess
fromList l = {-# SCC "Tree.fromList" #-}
  foldl
    ( \tree i ->
        set tree i (l !! fromIntegral i)
      )
    (empty64 (head l))
    [0 .. 63]
{-# INLINE fromList #-}

toList :: Tree a -> [a]
toList tree = {-# SCC "Tree.toList" #-} fmap (\i -> tree ?! i) [0..63]
{-# INLINE toList #-}