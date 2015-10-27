n = a `div` length xs
    where a = 10
          xs = [1, 2, 3, 4, 5]


mproduct :: [Int] -> Int
mproduct []     = 1
mproduct (x:xs) = x * mproduct xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where smaller = [y | y <- xs, y < x]
          larger  = [z | z <- xs, z > x]

rsort :: Ord a => [a] -> [a]
rsort []     = []
rsort (x:xs) = rsort larger ++ [x] ++ rsort smaller
    where smaller = [y | y <- xs, y <= x]
          larger  = [z | z <- xs, z >  x]

rqsort :: Ord a => [a] -> [a]
rqsort []     = []
rqsort (x:xs) = reverse (rqsort smaller ++ [x] ++ rqsort larger)
    where smaller = [y | y <- xs, y <= x]
          larger  = [z | z <- xs, z >  x]

rrsort :: Ord a => [a] -> [a]
rrsort []     = []
rrsort (x:xs) = reverse (reverse (rrsort smaller) ++ [x] ++ reverse (rrsort larger))
    where smaller = [y | y <- xs, y <= x]
          larger  = [z | z <- xs, z >  x]

msort :: Ord a => [a] -> [a]
msort []     = []
msort xs = msort larger ++ msort smaller ++ [x]
    where x       = minimum xs
          smaller = [y | y <- xs, y <= x]
          larger  = [z | z <- xs, z >  x]