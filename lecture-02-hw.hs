double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

myfun1 :: (a -> b) -> a -> b
myfun1 f x = f x

twice :: (a -> a) -> a -> a
twice f x = f (f x)

myfun2 :: [a] -> [a]
myfun2 xs = take 3 (reverse xs)