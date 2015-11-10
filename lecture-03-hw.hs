not'      :: Bool -> Bool
not' True  = False
not' False = True

signum :: Int -> Int
signum n | n < 0      = -1
         | n == 1     =  0 
         | otherwise  =  1


halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
	where n = length xs `div` 2         

halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = splitAt (div (length xs) 2) xs

safetail
  = \ xs ->
      case xs of
      	  [] -> []
      	  (_: xs) -> xs


--bit of sanity type checking,...

(|||) :: Bool -> Bool -> Bool
a ||| False = a
a ||| True  = True

remove :: Int -> [a] -> [a]
remove n xs = take n xs ++ drop (n + 1) xs


inta :: Integer
inta = 2

intb :: Num b => [[b]]
intb = [[1,2], [3,4]]

intc :: Num a => [[[a]]]
intc = [[[1,2,3]], [[4,5,6]]]

intd :: Integer -> Integer
intd x = x * 2

inte :: Num a => a -> a
inte x = x * 2

intf x = x * 2

e4 (x, y) = x

e6 [x, y] = (x, True)

e11 = ('\a', True) -- '\a' is an escaped ALERT
