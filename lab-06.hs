------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root         :: Rose a -> a 
root (x:>xs) =  x

children         :: Rose a -> [Rose a]
children (x:>xs) =  xs

_tree0 = 'x' :> map (flip (:>) []) ['a'..'x']

_tree1 = 'x' :> map (\c -> c :> []) ['a'..'A']

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs

-- ===================================
-- Ex. 3-7
-- ===================================

size           :: Rose a -> Int
size (_:>[])   =  1
size (_:>rs)   =  1 + foldl (+) 0 (map size rs)

leaves         :: Rose a -> Int
leaves (_:>[]) =  1
leaves (_:>rs) =  foldl (+) 0 (map leaves rs)

_tree3 = 1 :> map (\c -> c :> []) [1..5]

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  fmap f (r:>[]) = f r :> []
  fmap f (r:>rs) = f r :> map (fmap f) rs

-- Exercise 9 hint:
--
-- hugs> :t _f
-- hugs> Functor a => a b -> a b
--
-- Which is (obviously, but on second look) not the same as.
-- Functor f => f a -> f b
--
_f r = fmap head $ fmap (\x -> [x]) r  

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty  :: m
  mappend :: m -> m -> m

newtype Sum a     = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty      = Sum 0
  mappend l r = Sum (unSum l + unSum r)
  
instance Num a => Monoid (Product a) where
  mempty      = Product 1
  mappend l r = Product (unProduct l * unProduct r)

unSum         :: Sum a -> a
unSum (Sum x) =  x

unProduct             :: Product a -> a
unProduct (Product x) =  x

-- Exercise 12 hint:
--
-- hugs> :t Sum 3 `mappend` Sum 4
-- mappend (Sum 3) (Sum 4) :: Num a => Sum a
--
-- Which is (again obviously, but on second look) not the same as.
-- Int int => Sum int
--
-- But is the same as:
-- Num string => Sum string
--
-- Haskell TAs are Evil!
--

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold         :: Monoid m => f m -> m
  foldMap      :: Monoid m => (a -> m) -> (f a -> m)
  foldMap g as  =  fold $ fmap g as
  
instance Foldable Rose where
  fold (r:>[]) =  mappend r mempty
  fold (r:>rs) =  mappend r (foldl (mappend) mempty (map fold rs))

_tree14  = 1 :> [2 :> [], 3 :> [4 :> []]]
_tree14' = fmap Product _tree14

_tree16 = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]
  
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum as     = unSum $ foldMap Sum as
fproduct as = unProduct $ foldMap Product as

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

