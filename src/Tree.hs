{-# LANGUAGE DeriveGeneric #-}

-- moves 80% more memory and uses 15-33% more time in profiling, compared to Board.hs
module Tree (set, Board, (?!), empty64, searchIdx, diff) where

import Data.Bits (Bits (shift, testBit))
import Relude

data Board a = Leaf a | Node (Board a) (Board a)
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData)

instance Functor Board where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node x y) = Node (fmap f x) (fmap f y)

instance Semigroup (Board a) where
    (<>) = Node

instance Foldable Board where
    foldr f b (Leaf a) = f a b
    foldr f b (Node t1 t2) = b `seq` foldr f (foldr f b t2) t1

set :: Board a -> Word8 -> a -> Board a
set (Leaf _) _ y = Leaf y
set (Node l r) i y =
    let newI = shift i (-1)
     in if testBit i 0
            then Node l (set r newI y)
            else Node (set l newI y) r

(?!) :: Board a -> Word8 -> a
(?!) (Leaf x) _ = x
(?!) (Node l r) i =
    if testBit i 0
        then r ?! shift i (-1)
        else l ?! shift i (-1)

infixr 9 ?!

-- The two predicates are ANDed together - one based on Square and one based on Maybe Piece (could be smashed into one predicate, couldn't it?)
searchIdx :: NFData a => Board a -> (Word8 -> Bool) -> (a -> Bool) -> [(Word8, a)]
searchIdx tree' idxPred piecePred = go tree' idxPred piecePred []
  where
    go :: NFData a => Board a -> (Word8 -> Bool) -> (a -> Bool) -> [Bool] -> [(Word8, a)]
    go (Leaf x) ip pp path =
        let bitify = bits6toNum path
         in [(bitify, x) | pp x && ip bitify]
    go (Node l r) ip pp path =
        let a = go l ip pp (False : path)
            b = go r ip pp (True : path)
         in a <> b

-- in force a `par` force b `pseq` a <> b

bits6toNum :: [Bool] -> Word8
bits6toNum [a, b, c, d, e, f] =
    getSum $
        (if f then Sum 1 else Sum 0)
            <> (if e then Sum 2 else Sum 0)
            <> (if d then Sum 4 else Sum 0)
            <> (if c then Sum 8 else Sum 0)
            <> (if b then Sum 16 else Sum 0)
            <> (if a then Sum 32 else Sum 0)
bits6toNum x = error $ "Cannot consider " <> show x <> " as 6 bits"

empty64 :: a -> Board a
empty64 = go 6
  where
    go :: Word8 -> a -> Board a
    go 0 x = Leaf x
    go n x = Node (go (n - 1) x) (go (n - 1) x)

diff :: (NFData a, Eq a) => Board a -> Board a -> [(Word8, a)]
diff a b = searchIdx b (\w8 -> a ?! w8 /= b ?! w8) (const True)