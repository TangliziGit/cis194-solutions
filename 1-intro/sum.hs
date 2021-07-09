asum :: Int -> Int
asum 0 = 0
asum n = n + asum (n -1)

sumTwo :: [Int] -> [Int]
sumTwo []       = []
sumTwo [x]      = [x]
sumTwo (x:y:zs) = (x + y) : sumTwo zs
