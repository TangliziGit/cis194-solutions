-- ex5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi x a b c = hanoi (x-1) a c b ++ [(a, b)] ++ hanoi (x-1) c b a

hanoiLength :: Integer -> Integer
hanoiLength x = (fromIntegral . length) (hanoi x "a" "b" "c")

