import Data.Char

-- Exercise 1
--
putStr'        :: String -> IO ()
putStr' []     =  return ()
putStr' (x:xs) =  putChar x >> putStr' xs


-- Exersice 2
--
putStrLn'      :: String -> IO ()
putStrLn' []   =  putChar '\n'
putStrLn' xs   =  putStr' xs >> putStrLn' ""

-- OK
--putStrLn' []   =  putChar '\n'
--putStrLn' xs   =  putStr' xs >> putChar '\n'

-- OK
--putStrLn' []   =  putChar '\n'
--putStrLn' xs   =  putStr' xs >>= \ x -> putChar '\n'

-- NOK (does not compile)
--putStrLn' []   =  putChar '\n'
--putStrLn' xs   =  putStr' xs >> \ x -> putChar '\n'

-- OK
--putStrLn' []   =  putChar '\n'
--putStrLn' xs   =  putStr' xs >> putStr' "\n"

-- NOK (does not terminate)
--putStrLn' []   =  putChar '\n'
--putStrLn' xs   =  putStr' xs >> putStrLn' "\n"

-- NOK (does not terminate)
--putStrLn' []   =  return ""
--putStrLn' xs   =  putStrLn' xs >> putStr' "\n"

-- NOK (does not compile)
--putStrLn' []   =  putChar "\n"
--putStrLn' xs   =  putStr' xs >> \ x -> putChar '\n'


-- Exercise 3
--
getLine' :: IO String
getLine' =  get []

get     :: String -> IO String
get xs  =  do x <- getChar
              case x of
                '\n' -> return xs
                _    -> get (xs ++ [x])


-- Exercise 4
--
interact'   :: (String -> String) -> IO ()
interact' f =  do input <- getLine'
                  putStrLn' (f input)

toUpperCase    :: String -> String
toUpperCase xs = [toUpper x | x <- xs]

toLowerCase    :: String -> String
toLowerCase xs = [toLower x | x <- xs]


-- Exercise 5
--
sequence_'          :: Monad m => [m a] -> m ()
sequence_' ms       =  foldr (>>) (return ()) ms

-- NOK (does not compile)
--sequence_' []       =  return []
--sequence_' (m : ms) =  m >> \ _ -> sequence_' ms 

-- OK
--sequence_' []       =  return ()
--sequence_' (m : ms) =  (foldl  (>>) m ms) >> return ()

-- NOK (does not compile)
--sequence_' ms       =  foldl (>>) (return ()) ms

-- OK
--sequence_' []       =  return ()
--sequence_' (m : ms) =  m >> sequence_' ms

-- OK
--sequence_' []       =  return ()
--sequence_' (m : ms) =  m >>= \ _ -> sequence_' ms

-- NOK (does not compile)
--sequence_' ms       =  foldr (>>=) (return()) ms

-- OK
--sequence_' ms       =  foldr (>>) (return()) ms

-- NOK (does not compile)
--sequence_' ms       =  foldr (>>) (return []) ms


-- Exercise 6
--
sequence'          :: Monad m => [m a] -> m [a]
sequence' []       =  return []
sequence' (m : ms) =
  do a <- m
     as <- sequence' ms
     return (a : as)

-- OK
--sequence' []       =  return []
--sequence' (m : ms) =  m >>= \ a ->
--                              do as <- sequence' ms  
--                                 return (a : as) 

-- NOK (does not compile)
--sequence' ms       =  foldr func (return ()) ms
--  where
--    func :: (Monad m) => m a -> m [a] -> m [a]
--    func m acc = do x  <- m
--                    xs <- acc
--                    return (x : xs)

-- NOK (does not compile)
--sequence' ms = foldr func (return []) ms
--  where
--    func :: (Monad m) => m a -> m [a] -> m [a]
--    func m acc = m : acc

-- NOK (does not compile)
--sequence' []       =  return []
--sequence' (m : ms) =  return (a : as)
--  where
--    a  <- m
--    as <- sequence' ms

-- OK
--sequence' ms       =  foldr func (return []) ms
--  where
--    func :: (Monad m) => m a -> m [a] -> m [a]
--    func m acc = do x  <- m
--                    xs <- acc
--                    return (x : xs)

-- NOK (does not compile)
--sequence' []       =  return []
--sequence' (m : ms) = m >>
--  \ a ->
--    do as <- sequence' ms
--       return (a : as)

-- NOK (does not compile)
--sequence' []       =  return []
--sequence' (m : ms) = m >>= \ a ->
--       as <- sequence' ms
--       return (a : as)


-- Exercise 7
--
mapM'      :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as =  sequence' (map f as)

-- OK
--mapM' f as =  sequence' (map f as)

-- OK
--mapM' []   =  return []
--mapM' f (a : as) 
--  =  f a >>= \ b -> mapM' f as >>= \ bs -> return (b : bs)

-- NOK (does not compile)
--mapM' f as =  sequence_' (map f as)

-- NOK (does not compile)
--mapM' f [] =  return []
--mapM' f (a : as) =
--  do 
--     f a        -> b
--     mapM' f as -> bs
--     return (b : bs)

-- OK
--mapM' f [] =  return []
--mapM' f (a : as)
--  = do b  <- f a
--       bs <- mapM' f as
--       return (b : bs)

-- OK
--mapM' f [] =  return []
--mapM' f (a : as)
--  = f a >>=
--      \ b ->
--        do bs <- mapM' f as
--           return (b : bs)

-- NOK (reverses results)
--mapM' f [] =  return []
--mapM' f (a : as)
--  = f a >>=
--      \ b ->
--        do bs <- mapM' f as
--           return (bs ++ [b])


just a      = Just a
none a      = Nothing

singleton a = [a]
empty a     = []


-- Exercise 8
--
filterM'      :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] =  return []
filterM' p (x : xs)
  = do flag <- p x
       ys   <- filterM' p xs
       if flag then return (x : ys) else return ys 

maybeEven   :: Int -> Maybe Bool
maybeEven i =  if (i `mod` 2 == 0) then Just True else Just False


-- Exercise 9
--
foldLeftM            :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a []     =  return a 
foldLeftM f a (x:xs) =  f a x >>= \z -> foldLeftM f z xs

testFLM = foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r


-- Exercise 10
-- With help from: http://stackoverflow.com/questions/17055527/lifting-foldr-to-monad
--
foldRightM           :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f b []    =  return b
foldRightM f b (x:xs) = (\z -> f x z) <<= foldRightM f b xs
    where (<<=) = flip (>>=)

testFRM = foldRightM (\a b -> putChar a >> return (a : b)) [] (show [1,3..10]) >>= \r -> putStrLn r


-- Exersize 11
--
liftM     :: Monad m => (a -> b) -> m a -> m b
liftM f m =  do x <- m
                return (f x)

-- OK
--liftM f m =  do x <- m
--                return (f x)

-- NOK (does not compile)
--liftM f m =  m >>= \ a -> f a

-- OK
--liftM f m =  m >>= \ a -> return (f a)

-- NOK (does not compile)
--liftM f m =  return (f m)

-- NOK (does not compile)
--liftM f m =  m >>= \ a -> m >> \ b -> return (f a)

-- NOK (does not compile)
--liftM f m =  m >>= \ a -> m >> \ b -> return (f b)

-- NOK (does not compile)
--liftM f m =  mapM f [m]

-- NOK (does not compile)
--liftM f m =  m >> \ a -> return (f a)

testLM = liftM toUpper (Just 'a')                
