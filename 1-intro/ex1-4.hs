-- ex1
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0    = []
  | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- ex2
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[])= [x]
doubleEveryOtherRev (x:y:zs)= x : 2*y : doubleEveryOtherRev zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

-- ex3
sumDigit :: Integer -> Integer
sumDigit x
  | x <= 9    = x
  | otherwise = (x `mod` 10) + sumDigit (x `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:ys) = sumDigit x + sumDigits ys

-- ex4
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0
