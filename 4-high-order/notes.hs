qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort left ++ [x] ++ qsort right
  where left  = filter (<x) xs
        right = filter (>=x) xs

-- ($) :: (a -> b) -> a -> b 	-- Defined in ‘GHC.Base’
-- infixr 0 $

-- why (a -> b) -> a?
-- toStr $ sum3 1 2 3
--  toStr 1 2 3 s3 $
--  toStr 6 $
--  "6"
--
-- toStr s3 1 2 3
--  toStr 1 2 3 s3
--  toStr 6
--  !!!Error
--
-- why infixR?
-- toStr $ s3 1 2 $ 3 + 0
--  toStr 1 2 s3 3 0 + $ $
--  toStr 1 2 s3 3 $ $
--  toStr 1 2 (s3 3) $
--  toStr 6 $
--  "6"
--
-- why infixr **0**?
-- it performs like `)`, 
-- toStr ( sum3 1 2 3 )
--  1 2 3 sum3 toStr


-- (.) :: (b -> c) -> (a -> b) -> a -> c 	-- Defined in ‘GHC.Base’
-- infixr 9 .

-- add1 . add2 2
--
