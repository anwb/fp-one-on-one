-- Exercise 9
--
_length        :: [a] -> Int
_length []     =  0
_length (x:xs) =  1 + _length xs

_take               :: Int -> [a] -> [a]
_take 0     _       =  []
_take (n+1) []      =  []
_take (n+1) (x: xs) =  x : _take n xs

_repeat   :: a -> [a]
_repeat x =  x : _repeat x

_test9     :: Int -> a -> Int
_test9 n x =  _length (_take n (_repeat x))