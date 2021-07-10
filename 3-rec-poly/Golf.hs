module Golf where

-- ex1
pickEvery :: Int -> [a] -> [a]
pickEvery n xs = [xs !! i | i <- [n-1, 2*n-1 .. length xs - 1]]

skips :: [a] -> [[a]]
skips xs = [pickEvery i xs | i <- [1..length xs]]

-- ex2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rst)
  | x < y && y > z  = y : localMaxima (y:z:rst)
  | otherwise       = localMaxima (y:z:rst)
localMaxima _ = []

-- ex3
bottom :: String
bottom = "==========\n0123456789\n"

counts :: [Integer] -> [Int]
counts xs = map (\x -> (length . filter (==x)) xs) [0..9]

line :: [Int] -> Int -> String
line cnts n = [ if c < n then ' ' else '*' | c <- cnts]

histogram :: [Integer] -> String
histogram xs = unlines ( map (line cnts) [maxCount, maxCount-1 .. 1] ) ++ bottom
  where 
    cnts = counts xs
    maxCount = maximum cnts

