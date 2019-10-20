{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Tree where

import Data.Bits
import Data.Word
import Control.DeepSeq
import Control.Parallel
import GHC.Generics

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show, Generic, NFData)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x y) = Node (fmap f x) (fmap f y)

instance Semigroup (Tree a) where
  (<>) = Node

instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Node t1 t2) = foldr f (foldr f b t2) t1

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
{-# INLINE set #-}

(?!) :: Tree a -> Word8 -> a
(?!) (Leaf x) _ = x
(?!) (Node l r) i = {-# SCC "Tree.?!" #-}
  if testBit i 0
    then r ?! shift i (-1)
    else l ?! shift i (-1)
{-# INLINE (?!) #-}

infixr 9 ?!

search :: Tree a -> (a -> Bool) -> [a]
search (Leaf x) pred' = [x | pred' x]
search (Node l r) pred' = {-# SCC "Tree.search" #-} search l pred' <> search r pred'
{-# INLINE search #-}

-- The two predicates are ANDed together - one based on Square and one based on Maybe Piece (could be smashed into one predicate, couldn't it?)
searchIdx :: Tree a -> (Word8 -> Bool) -> (a -> Bool) -> [(Word8, a)]
searchIdx tree' idxPred piecePred = {-# SCC "Tree.searchIdx" #-} go tree' idxPred piecePred []
  where go :: Tree a -> (Word8 -> Bool) -> (a -> Bool) -> [Bool] -> [(Word8, a)]
        go (Leaf x) ip pp path = {-# SCC "Tree.searchIdx.go.Leaf" #-} [(bits6toNum path, x) | ip (bits6toNum path) && pp x]
        go (Node l r) ip pp path = {-# SCC "Tree.searchIdx.go.Node" #-} go l ip pp (False : path) <> go r ip pp (True : path)
{-# INLINE searchIdx #-}

-- slooooow
-- The two predicates are ANDed together - one based on Square and one based on Maybe Piece (could be smashed into one predicate, couldn't it?)
searchIdx2 :: Tree a -> (Word8 -> Bool) -> (a -> Bool) -> [(Word8, a)]
searchIdx2 tree' idxPred piecePred = {-# SCC "Tree.searchIdx2" #-}
  filter idxPred [0 .. 63]
  >>= ( \i ->
          let mp = tree' ?! i
            in [(i, mp) | piecePred mp])
{-# INLINE searchIdx2 #-}

bits6toNum :: [Bool] -> Word8
bits6toNum bits = {-# SCC "Tree.toNum" #-} go bits 5 0
  where go :: [Bool] -> Word8 -> Word8 -> Word8
        go (b:bs) pos acc = (if b then 2^pos else 0) + go bs (pred pos) acc 
        go [] _ acc = acc
{-# INLINE bits6toNum #-}

empty64 :: a -> Tree a
empty64 = {-# SCC "Tree.empty64.go" #-} go 6
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
toList tree = {-# SCC "Tree.toList" #-} fmap (\i -> tree ?! i) [0..63] -- todo Ttraverse tree directly
{-# INLINE toList #-}