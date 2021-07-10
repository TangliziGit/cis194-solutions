module Homework4 where 

import Data.List

-- ex1

-- fun1 :: [Integer] -> Integer
-- fun1 []     = 1
-- fun1 (x:xs)
--   | even x    = (x - 2)*fun1 xs
--   | otherwise = fun1 xs

-- TODO: why $ can not use? how to simplify (-2)?
fun1' :: [Integer] -> Integer
fun1' = product . (map (\x -> x-2)) . (filter even)


-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n 
--   | even n    = n + fun2 (n `div` 2)
--   | otherwise = fun2 (3*n + 1)

fun2' :: Integer -> Integer
fun2' = sum . (filter even) . (takeWhile (>1)) . (iterate convert)
  where convert x
          | even x    = x `div` 2
          | otherwise = 3*x + 1


-- ex2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert' Leaf

insert' :: a -> Tree a -> Tree a
insert' a Leaf = Node 0 Leaf a Leaf
insert' a (Node 0 Leaf root Leaf) = Node 1 (insert' a Leaf) root Leaf
insert' a (Node h Leaf root r) = Node h (insert' a Leaf) root r
insert' a (Node h l root Leaf) = Node h l root (insert' a Leaf)
insert' a (Node h left@(Node hl _ _ _) root right@(Node hr _ _ _))
  | hl < hr   = Node h (insert' a left) root right
  | hl > hr   = Node h left root (insert' a right)
  | otherwise = Node h' left' root right
  where 
    left' = insert' a left
    h'    = height left' + 1

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h


-- ex3
xor :: [Bool] -> Bool
xor = odd . length . (filter (==True))

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

-- ex4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [3..(2*n+2)] \\ unprimes
  where unprimes = filter (<=2*n+2) [i*j | i <- [2..n], j <- [2..n]]
