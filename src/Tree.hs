{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Tree where

import Control.DeepSeq
import Data.Bits
import Data.Monoid
import Data.Word
import GHC.Generics

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show, Generic, NFData)

instance Functor Tree where
  fmap f (Leaf !x) = Leaf (f x)
  fmap f (Node !x !y) = Node (fmap f x) (fmap f y)

instance Semigroup (Tree a) where
  (<>) = Node

instance Foldable Tree where
  foldr f b (Leaf !a) = f a b
  foldr f b (Node !t1 !t2) = foldr f (foldr f b t2) t1

set :: Tree a -> Word8 -> a -> Tree a
set (Leaf _) _ y = Leaf y
set (Node !l !r) i y =
  let newI = shift i (-1)
   in if testBit i 0
        then Node l (set r newI y)
        else Node (set l newI y) r
{-# INLINE set #-}

(?!) :: Tree a -> Word8 -> a
(?!) (Leaf !x) _ = x
(?!) (Node !l !r) i =
  if testBit i 0
    then r ?! shift i (-1)
    else l ?! shift i (-1)
{-# INLINE (?!) #-}

infixr 9 ?!

search :: Tree a -> (a -> Bool) -> [a]
search (Leaf !x) pred' = [x | pred' x]
search (Node !l !r) pred' = search l pred' <> search r pred'
{-# INLINE search #-}

-- The two predicates are ANDed together - one based on Square and one based on Maybe Piece (could be smashed into one predicate, couldn't it?)
searchIdx :: Tree a -> (Word8 -> Bool) -> (a -> Bool) -> [(Word8, a)]
searchIdx !tree' !idxPred !piecePred = go tree' idxPred piecePred []
  where
    go :: Tree a -> (Word8 -> Bool) -> (a -> Bool) -> [Bool] -> [(Word8, a)]
    go (Leaf !x) !ip !pp !path = [(bits6toNum path, x) | ip (bits6toNum path) && pp x]
    go (Node !l !r) !ip !pp !path = go l ip pp (False : path) <> go r ip pp (True : path)
{-# INLINE searchIdx #-}

bits6toNum :: [Bool] -> Word8
bits6toNum [a,b,c,d,e,f] = getSum $
  ( if f then Sum 1 else Sum 0 ) <>
  ( if e then Sum 2 else Sum 0 ) <>
  ( if d then Sum 4 else Sum 0 ) <>
  ( if c then Sum 8 else Sum 0 ) <>
  ( if b then Sum 16 else Sum 0 ) <>
  ( if a then Sum 32 else Sum 0 )
{-# INLINE bits6toNum #-}

bits6toNum' :: [Bool] -> Word8
bits6toNum' !bits = go bits 5 0
  where
    go :: [Bool] -> Word8 -> Word8 -> Word8
    go (b : bs) !pos !acc = (if b then 2 ^ pos else 0) + go bs (pred pos) acc
    go [] _ !acc = acc
{-# INLINE bits6toNum' #-}

empty64 :: a -> Tree a
empty64 = go 6
  where
    go :: Word8 -> a -> Tree a
    go 0 !x = Leaf x
    go !n !x = Node (go (n - 1) x) (go (n - 1) x)
{-# INLINE empty64 #-}

fromList :: [a] -> Tree a -- 64 Leafs.. sorry mac, that's what you get - this is chess
fromList !l =
  foldl
    ( \tree i ->
        set tree i (l !! fromIntegral i)
    )
    (empty64 (head l))
    [0 .. 63]
{-# INLINE fromList #-}

toList :: Tree a -> [a]
toList !tree = fmap (tree ?!) [0 .. 63] -- todo Ttraverse tree directly
{-# INLINE toList #-}

diff :: Eq a => Tree a -> Tree a -> [(Word8, a)]
diff !a !b = searchIdx b (\w8 -> a ?! w8 /= b ?! w8) (const True)
{-# INLINE diff #-}