module HuttonMonads where
import Prelude hiding (map, Maybe, Nothing, Just, (>>=), Monad)

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
(>>=)         :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing  >>= _ = Nothing
(Just x) >>= f = f x      

eval'''          :: Expr -> Maybe Int
eval''' (Val n)   = Just n            
eval''' (Div x y) = eval''' x >>= \n ->
                    eval''' y >>= \m ->
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
class Monad m where
  return :: a -> m a
  (>>>=) :: m a -> (a -> m b) -> m b

instance Monad Maybe where
  -- return      :: a -> m a
  return   x      = Just x

  -- (>>>=)      :: m a -> (a -> m b) -> m b
  Nothing  >>>= _ = Nothing
  (Just x) >>>= f = f x
