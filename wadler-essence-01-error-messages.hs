-- The Essence of Functional Programming
-- P. Wadler, 1992
-- 

-- Test data
-- 
-- ((\x.x+x)(10+11))
--
expr = (App (Lam "x" (Add (Var "x") (Var "x")))(Add (Con 10) (Con 11)))


type  Name            = String

data  Term            =  Var Name
                      |  Con Int
                      |  Add Term Term
                      |  Lam Name Term
                      |  App Term Term

data Value            =  Wrong
                      |  Num Int
                      |  Fun (Value -> E Value)

type  Environment     =  [(Name, Value)]

showval               :: Value -> String
showval Wrong         =  "<wrong>"
showval (Num i)       =  show i
showval (Fun f)       =  "<function>"

interp                :: Term -> Environment -> E Value
interp (Var x) e      =  findin x e
interp (Con i) e      =  unitE (Num i)
interp (Add u v) e    =  interp u e `bindE` (\a ->
                         interp v e `bindE` (\b ->
                         add a b))
interp (Lam x v) e    =  unitE (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e    =  interp t e `bindE` (\f ->
                         interp u e `bindE` (\a ->
                         apply f a))

findin                :: Name -> Environment -> E Value
findin x []           =  errorE ("unbound variable: " ++ x)
findin x ((y,b):e)    =  if x==y then unitE b  else  findin x e

add                   :: Value -> Value -> E Value
add (Num i) (Num j)   =  unitE (Num (i+j))
add a b               =  errorE ("should be numbers: " ++ showval a
                                               ++ ", " ++ showval b)

apply                 :: Value -> Value -> E Value
apply (Fun k) a       =  k a
apply f a             =  errorE ("should be function: " ++ showval f)

test                  :: Term -> String
test t                =  showE (interp t [])


-- Variation 1        :  Error Messages - Error Monad
--
data   E a            =  Success a | Error String

unitE  a              =  Success a
errorE s              =  Error s

(Success a) `bindE` k =  k a
(Error s)   `bindE` k =  Error s

showE (Success a)     =  "Success: " ++ showval a
showE (Error s)       =  "Error: " ++ s

