-- Exercise 6
--
fibs :: [Integer]
fibs =  0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

-- Exercise 7
--
fib   :: Int -> Integer
fib n =  fibs !! n 

-- NOK (off by one)
-- fib n = head (drop (n-1) fibs)

-- NOK (off by one)
-- fib n = last (take n fibs)

-- NOK (does not compile)
--fib n = index fibs n
