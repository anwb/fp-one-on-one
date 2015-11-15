type Church a = (a -> a) -> a -> a

zero :: Church a
zero = \s z -> z

one :: Church a
one = \s z -> s z

two :: Church a
two = \s z -> (s . s) z

-- lambda eta reduction, gets rid of z
three :: Church a
three = \s -> (s . s . s)                              

church2string :: Church String -> String
church2string x = x ('*':) ""

church2int :: Church Integer -> Integer
church2int x = x (+1) 0


-- operators

-- implement addition starting from y as the new z for x
cadd :: Church a -> Church a -> Church a
cadd x y = \s z -> x s (y s z)                          

-- implement with y times s as the super-succ function
-- \s z -> (x . y) s z :: \s z -> x (y s) z 
cmul :: Church a -> Church a -> Church a
cmul x y = x . y

-- does not compile
--cexp :: Church a -> Church a -> Church a
--cexp x y = y x


