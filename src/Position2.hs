module Position2 where

import Data.Bits
import Data.Word

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)

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

(>.) :: Tree a -> Word8 -> a -> Tree a
(>.) (Leaf x) _ y = Leaf y
(>.) (Node l r) i y =
  let newI = shift i (-1)
  in if testBit i 0
    then
      let newR = (>.) r newI y
       in Node l newR
    else
      let newL = (>.) l newI y
       in Node newL r
infixl 9 >.

(?) :: Tree a -> Word8 -> a
(?) (Leaf x) _ = x
(?) (Node l r) i = if testBit i 0
  then r ? shift i (-1)
  else l ? shift i (-1)
infixr 9 ?

empty64 :: a -> Tree a
empty64 = go 6
  where go :: Word8 -> a -> Tree a
        go 0 x = Leaf x
        go n x = Node (go (n - 1) x) (go (n - 1) x)

fromList :: [a] -> Tree a -- 64 Leafs.. sorry mac, that's what you get - this is chess
fromList l = foldl (\tree i -> (>.) tree i (l !! fromIntegral i)) (empty64 (head l)) [0 .. 63]