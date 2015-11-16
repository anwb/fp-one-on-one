module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens = \xs -> [x | x <- xs, x `mod` 2 == 0]

-- ===================================
-- Ex. 3 - 4 
-- ===================================

-- complete the following line with the correct type signature for this function
squares :: Integer -> [Integer]
squares n = [x * x | x <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
squares' :: Integer -> Integer -> [Integer]
squares' m n = [x * x | x <- [i..j]]
  where
    i = n + 1
    j = m + n

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords n m = [ (x,y) | x <- [0..n], y <- [0..m] ]
