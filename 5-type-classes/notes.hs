data Foo = I Int | D Double
  deriving (Show, Ord)

instance Eq Foo where
  (I x) == (I y) = x == y
  (D x) == (D y) = x == y

