module Main where
import Data.List
import Data.Char
import Hugs.IOExts (unsafeCoerce)

data Nat = Zero
         | Succ Nat
         deriving Show

-- Exercise 0
--
natToInteger          :: Nat -> Integer
natToInteger Zero     =  0
natToInteger (Succ n) =  natToInteger n + 1

-- OK
--natToInteger (Succ n) =  natToInteger n + 1
--natToInteger Zero     =  0

-- NOK (does not terminate)
--natToInteger n = natToInteger n

-- OK
--natToInteger Zero     =  0
--natToInteger (Succ n) =  1 + natToInteger n

-- NOK (always returns 1)
--natToInteger Zero     =  1
--natToInteger (Succ n) =  (1 + natToInteger n) - 1

-- OK 
--natToInteger =  head . m
--  where m Zero     =  [0]
--        m (Succ n) =  [sum [x | x <- (1 : m n)]]

-- OK
--natToInteger =  \ n -> genericLength [c | c <- show n, c == 'S' ]

-- NOK (does not typecheck as length returns an Int instead of Integer)
--natToInteger =  \ n -> length [c | c <- show n, c == 'S' ]


-- Exercise 1
--
integerToNat :: Integer -> Nat
integerToNat 0     =  Zero
integerToNat (n+1) =  Succ (integerToNat n)

-- OK
--integerToNat 0     =  Zero
--integerToNat (n+1) =  Succ (integerToNat n)

-- NOK (infinit recursion on pattern match when n > 0)
--integerToNat 0 =  Zero
--integerToNat n =  (Succ (integerToNat n))

-- NOK (does not compile)
--integerToNat n =  product [(unsafeCoerce c) :: Integer | c <- show n]

-- NOK (does not terminate)
--integerToNat n =  integerToNat n

-- OK
--integerToNat (n+1) =  Succ (integerToNat n)
--integerToNat 0     =  Zero

-- OK
--integerToNat (n+1) =  let m = integerToNat n in Succ m
--integerToNat 0     =  Zero

-- NOK (does not compile)
--integerToNat = head . m
--  where {
--        ; m 0       = [0]
--        ; m (n + 1) = [sum [x | x <- (1 : m n)]]
--        }

-- NOK (does not compile)
--integerToNat = \ n -> genericLength [c | c <- show n, isDigit n]


-- Exercise 2
--
add            :: Nat -> Nat -> Nat
add Zero n     =  n
add (Succ m) n =  Succ (add n m)   

-- OK
--add (Succ m) n =  Succ (add n m)   
--add Zero n     =  n

-- NOK
--add Zero n     =  Zero
--add (Succ m) n =  Succ (add m n)   

-- NOK
--add (Succ m) n =  Succ (add m n)   
--add Zero n     =  Zero

-- NOK
--add n Zero     =  Zero
--add n (Succ m) =  Succ (add n m)   

-- NOK
--add n (Succ m) =  Succ (add n m)   
--add n Zero     =  Zero

-- OK
--add n Zero     =  n
--add n (Succ m) =  Succ (add m n)   

-- OK
--add n (Succ m) =  Succ (add m n)   
--add n Zero     =  n


-- Exercise 3
--
mult            :: Nat -> Nat -> Nat
mult m Zero     =  Zero
mult m (Succ n) =  add m (mult m n)


-- Exercise 4
--
data Tree = Leaf Integer
          | Node Tree Integer Tree
          deriving Show

occurs            :: Integer -> Tree -> Bool
occurs m (Leaf n) =  m == n
occurs m (Node l n r)
  = case compare m n of
      LT -> occurs m l
      EQ -> True
      GT -> occurs m r

-- NOK (when ordered left to right)
--occurs m (Leaf n) =  m == n
--occurs m (Node l n r)
--  = case compare m n of
--      LT -> occurs m r
--      EQ -> True
--      GT -> occurs m l

-- NOK (does not type check on leaf pattern)
--occurs m (Leaf n) =  compare m n
--occurs m (Node l n r)
--  = case compare m n of
--      LT -> occurs m l
--      EQ -> True
--      GT -> occurs m r

-- NOK (does not handle the node equals case correctly)
--occurs m (Leaf n) =  m == n
--occurs m (Node l n r)
--  = case compare m n of
--      LT -> occurs m l
--      EQ -> False
--      GT -> occurs m r

-- OK
--occurs m (Leaf n) =  m == n
--occurs m (Node l n r)
--  | m == n    = True
--  | m < n     = occurs m l
--  | otherwise = occurs m r

-- NOK (when ordered left to right)
--occurs m n =  m == n
--occurs m (Node l n r)
--  | m == n    = True
--  | m > n     = occurs m l
--  | otherwise = occurs m r

-- NOK (does not type check)
--occurs m n =  m == n
--occurs m (Node l n r)
--  | m == n    = False
--  | m < n     = occurs m l
--  | otherwise = occurs m r


-- test case
lbtree = Node (Node (Leaf (-1)) 0 (Node (Leaf 1) 3 (Leaf 4))) 5 (Leaf 7)


-- Exercise 5
--
data BTree = BLeaf Integer
           | BNode BTree BTree
           deriving Show

leaves               :: BTree -> Integer
leaves (BLeaf _)     = 1
leaves (BNode l r)   = leaves l + leaves r

balanced             :: BTree -> Bool
balanced (BLeaf _)   =  True          
balanced (BNode l r) =  abs (leaves l - leaves r) <= 1 && balanced l && balanced r

-- test case
bbtree = BNode (BNode (BLeaf 0) (BLeaf 1)) (BNode (BLeaf 4) (BLeaf 7))


-- Exercise 6
--
balance     :: [Integer] -> BTree
halve xs = splitAt (length xs `div` 2) xs
balance [x] = BLeaf x
balance xs  = BNode (balance ys) (balance zs)
  where (ys,zs) = halve xs


-- Exercise 7
--
typecheck7 = Add7 (Val7 1) (Val7 2)
data Expr7 = Add7 Expr7 Expr7 | Val7 Int


-- Exercise 8
--
typecheck8 = Node8 (Leaf8 1) (Leaf8 2)
data Tree8 = Leaf8 Int | Node8 Tree8 Tree8


-- Exercise 9
--
data Maybe9 a = Nothing9 | Just9 a

instance Monad Maybe9 where
  return x       = Just9 x
  Nothing9 >>= _ = Nothing9
  Just9 x  >>= f = f x

test9just2   = fmap (+1) (Just 1)
test9nothing = fmap (+1) Nothing


-- Exercise 10
--
--instance Monad [] where
--  return x = [x]
--  xs >>= f = concat (map f xs) 

-- Reflect whether or not lists are still monads when we allow partial,
-- bottom, and infinite lists.


-- Exercise 11
--
--instance Monoid [a] where
--  mempty = []
--  (<>)   = (++)

-- prove that in your chosen implementation that (<>) and mempty in fact
-- satisfy the extra algebraic laws required for monoids. In practice
-- typically we ignore bottom and whether or not the algebraic laws hold.


-- Exercise 12
--
-- instance Functor Maybe where
--   fmap _ Nothing  = Nothing
--   fmap f (Just x) = Just (f x)


-- Exercise 13
--
-- instance Foldable [] where
--   fold = foldr (<>) mempty