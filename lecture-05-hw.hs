(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m * m ^^^ (n - 1)

(^^^^) :: Int -> Int -> Int
m ^^^^ 0 = 1
m ^^^^ n = m * (^^^^) m (n - 1)

mand []     = True
mand (x:xs) = x && mand xs

nand []     = True
nand (x:xs)
  | x = and xs
  | otherwise = False 

oand [] = True
oand (x:xs)
  | x == False = False
  | otherwise = oand xs

pand [] = True
pand (x:xs) = pand xs && x