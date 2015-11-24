module HuttonMonads where
import Prelude hiding (map, Maybe, Nothing, Just)


-- Part 1 : Abstracting programming patterns
--
inc       :: [Int] -> [Int]
inc []     = []
inc (x:xs) = x + 1 : inc xs

sqr       :: [Int] -> [Int]
sqr []     = []
sqr (x:xs) = x ^ 2 : sqr xs

map         :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

inc' = map (+1)
sqr' = map (^2) 


-- A simple evaluator

data Expr = Val Int | Div Expr Expr
  deriving Show

eval          :: Expr -> Int
eval (Val n)   = n
eval (Div x y) = eval x `div` eval y

-- With safe devision
--
data Maybe a = Nothing | Just a
  deriving Show

safeDiv    :: Int -> Int -> Maybe Int   
safeDiv n m = if m == 0 then Nothing else Just (n `div` m)

eval'          :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = case eval' x of
                    Nothing -> Nothing
                    Just n  -> case eval' y of
                                 Nothing -> Nothing
                                 Just m  -> safeDiv n m

-- With sequencing two values of Maybe
--
seqn                  :: Maybe a -> Maybe b -> Maybe (a,b)
seqn Nothing  _        = Nothing
seqn _        Nothing  = Nothing
seqn (Just x) (Just y) = Just (x,y)

apply           :: (a -> Maybe b) -> Maybe a -> Maybe b
apply f Nothing  = Nothing
apply f (Just x) = f x

eval''          :: Expr -> Maybe Int
eval'' (Val n)   = Just n
eval'' (Div x y) = apply f (eval'' x `seqn` eval'' y)
                   where f (n,m) = safeDiv n m

-- Combining sequencing and processing
--
(>>>=)         :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >>>= _ = Nothing
(Just x) >>>= f = f x      

eval'''          :: Expr -> Maybe Int
eval''' (Val n)   = Just n            
eval''' (Div x y) = eval''' x >>>= \n ->
                    eval''' y >>>= \m ->
                    safeDiv n m         

{- Exercises
--
eval (Div x y) = eval x >>= \n ->
                 eval y >>= \m ->
                 safeDiv n m

= { case expansion of >>= }  

eval (Div x y) = case eval x of
                  Nothing -> Nothing
                  Just n  -> case eval y of
                              Nothing -> Nothing
                              Just m  -> safeDiv n m

--
seqn         :: a -> Maybe a
seqn Nothing  = Nothing
seqn x        = Just x 

eval           :: Expr -> Mabe Int 
eval (Op x y z) = do m <- seqn (eval x)
                     n <- seqn (eval y)
                     o <- seqn (eval z)
                     f m n o = ...
-}


-- Part 2 : Monads in Haskell
--

-- Base.Monad is defined as
--
-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

-- Maybe monad
--
instance Monad Maybe where
  -- return      :: a -> m a
  return   x      = Just x

  -- (>>=)      :: m a -> (a -> m b) -> m b
  Nothing  >>= _ = Nothing
  (Just x) >>= f = f x

-- List monad defined in GHC.Base
--
-- instance Monad [] where
--   -- return  :: a -> m a
--   return   x  = [x]
--   -- (>>=) :: m a -> (a -> m b) -> m b
--   xs  >>= f = concat (map f xs)

-- Use case of list monad
--
pairs       :: [a] -> [b] -> [(a,b)]
pairs xs ys  = do x <- xs
                  y <- ys
                  return (x,y)  

-- Similarly implemented using list comprehension
pairs'       :: [a] -> [b] -> [(a,b)]
pairs' xs ys  = [(x,y) | x <- xs, y <- ys]


-- State monad
--
type State = Int

newtype ST a = S (State -> (a, State))

applyS         :: ST a -> State -> (a, State)
applyS (S f) x  = f x

instance Monad ST where
  --   |                 ^
  -- x |    +-------+    | x 
  --   `----|-------|----'
  --        |       |
  -- -------|-------|------>
  --    s   +-------+   s  
  --
  -- return :: a -> ST a
  return x   = S (\s -> (x,s))

  --                                       ^ 
  --         +-------+   x    +-------+    |
  --    s    |       | -----> |       | ---'
  --  -----> |  st   |        |   f   |
  --         |       | -----> |       | ----->
  --         +-------+   s'   +-------+
  --          
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f  = S (\s -> let (x,s') = applyS st s in applyS (f x) s')

-- An example labeling binary tree leaves with "fresh" integers
--
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

tree :: Tree Char
tree  = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

fresh :: ST Int
fresh  = S (\n -> (n , n+1))

mlabel           :: Tree a -> ST (Tree (a,Int))
mlabel (Leaf x)   = do n <- fresh
                       return (Leaf (x,n))
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

label   :: Tree a -> Tree (a,Int)
label t  = fst (applyS (mlabel t) 0)    

-- Excersises
--
app   :: (State -> State) -> ST State
app f  =  S (\s -> (s, f s))

fresh' :: ST Int
fresh'  = app (+1) 

mlabel'           :: Tree a -> ST (Tree (a,Int))
mlabel' (Leaf x)   = do n <- fresh'
                        return (Leaf (x,n))
mlabel' (Node l r) = do l' <- mlabel' l
                        r' <- mlabel' r
                        return (Node l' r')

run    :: ST a -> State -> a
run st  = \s -> fst (applyS st s)

label'   :: Tree a -> Int -> Tree (a,Int)
label' t  = \s -> run (mlabel' t) s

-- Idea for Exercide: Label a tree with uuid's
-- fuseOf:
--    o  FPS RNG State Monad Example.
--    o  UUID RFC
--    o  The fresh int example and exercise above.
--

-- Not understood yet:
-- allOf:
--   o   The interaction ST irt. S and applyS 
--   o   The interaction (>>=) and return irt. the do notation
--       - Note: strange, as we do understand scala's map and flatMap
--         irt. how for comprehensions are compiled into the same
--         two functions.  Wasn't (>>=) supposed to be another
--         symbol for scala's `flatMap` function?  And, What is the
--         haskell symbol for scala's `map` function?

-- Part 3 : The IO Monad
--






