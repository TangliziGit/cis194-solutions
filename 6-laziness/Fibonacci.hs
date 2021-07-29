{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- ex1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = (fib (x - 1)) + (fib (x - 2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- ex2
fibs2 :: [Integer]
fibs2 = [0, 1] ++ [ fibs2!!(i-1) + fibs2!!(i-2) | i <- [2..]]

-- ex3
-- intresting, one data without the way to build without itself
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- ex4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

streamFromSeed :: (a -> a) -> a -> Stream a 
streamFromSeed f a = Cons a $ streamFromSeed f (f a)

-- ex5
nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) bs = Cons a $ interleaveStreams bs as

-- you can't write the code below, pattern matching should be less
-- interleaveStreams (Cons a as) (Cons b bs) = Cons a $ Cons b $ interleaveStreams as bs


ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) $ streamMap (+1) ruler

-- how `ruler` works?
-- ruler = map power $ [1..]
--       = map power $ [1,3..] ++ (map (*2) [1..])
--       = map power $ [1,3..] ++ (map (*2) [1,3..]) ++ (map (*4) [1,3..]) ++ ...
--       = inter [0,0..] . inter [1,1..] . inter [2,2..] . inter [3,3..] ...
--       = inter [0,0..] $ map (+1) ruler

-- ex6
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate = streamMap (*(-1))
  (+) (Cons a as) (Cons b bs) = Cons (a+b) $ as + bs
  (*) (Cons a as) s@(Cons b bs) = Cons (a*b) $ streamMap (*a) bs + as*s

instance Fractional (Stream Integer) where
  (/) (Cons a as) (Cons b bs) = res
    where res = Cons (a `div` b) $ streamMap (`div` b) (as - res * bs)

-- generating function, awesome
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- ex7
data Matrix = Matrix Integer Integer Integer Integer
  deriving Show

instance Num Matrix where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    Matrix (a11 * b11 + a12 * b21) (a11 * b21 + a12 * b22)
           (a21 * b11 + a22 * b21) (a21 * b21 + a22 * b22)

-- Haskell already using binary exponentiation
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = ans $ seed^n
  where ans = (\(Matrix _ a12 _ _) -> a12)
        seed = Matrix 1 1 1 0
