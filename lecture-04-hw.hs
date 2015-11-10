import Data.Char

factors :: Int -> [Int] 
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [ x | x <- [2..n], prime x ] 

sorted :: Ord a => [a] -> Bool
sorted xs = and [ x <= y | (x, y) <- zip xs (tail xs) ]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

sum100 :: Int
sum100 = sum [x ^ 2 | x <- [1..100]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect m = sum (init (factors m)) == m

--Cypher
let2int :: Char -> Int
let2int c = ord c - ord 'a'

cap2int :: Char -> Int
cap2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2cap n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = int2cap ((cap2int c + n) `mod` 26)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


--Devides
divides :: Int -> Int -> Bool
divides n m = n `mod` m == 0

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `divides` x]

