-- With help from: http://www.cs.nott.ac.uk/~psznhn/MGS2012/LectureNotes/lecture05-9up.pdf

module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

action                :: Concurrent a -> Action
action (Concurrent x) =  x (const Stop)

-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop =  Concurrent (\_ -> Stop)


-- ===================================
-- Ex. 2
-- ===================================

atom   :: IO a -> Concurrent a
atom m =  Concurrent (\c -> Atom (m >>= (\a -> return (c a))))

-- ===================================
-- Ex. 3
-- ===================================

fork   :: Concurrent a -> Concurrent ()
fork m =  Concurrent (\c -> Fork (action m) (c ()))

par                               :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent x) (Concurrent y) =  Concurrent (\c -> (Fork (x c) (y c)))


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    return x = Concurrent (\c -> c x)
    (Concurrent f) >>= g = Concurrent (\c1 -> f (\c2 -> concurrent (g c2) c1))
        where concurrent (Concurrent x) = x


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin []     = return ()
roundRobin (x:xs) = case x of
    Atom am    -> do { a' <- am ; roundRobin (xs ++ [a']) }
    Fork a1 a2 -> roundRobin (xs ++ [a1,a2])
    Stop       -> roundRobin xs 


-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

