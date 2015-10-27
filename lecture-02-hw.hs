double x = x * 2
palindrome xs = reverse xs == xs
myfun f x = f x
twice f x = f (f x)
f xs = take 3 (reverse xs)