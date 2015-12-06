-- The Essence of Functional Programming
-- P. Wadler, 1992
-- 

-- Test data
-- 
-- ((\x.x+x)(10+11))
--
expr = (App (Lam "x" (Add (Var "x") (Var "x")))(Add (Con 10) (Con 11)))


type  Name          = String

data  Term          =  Var Name
                    |  Con Int
                    |  Add Term Term
                    |  Lam Name Term
                    |  App Term Term

data Value          =  Wrong
                    |  Num Int
                    |  Fun (Value -> I Value)

type  Environment   =  [(Name, Value)]

showval             :: Value -> String
showval Wrong       =  "<wrong>"
showval (Num i)     =  show i
showval (Fun f)     =  "<function>"

interp              :: Term -> Environment -> I Value
interp (Var x) e    =  findin x e
interp (Con i) e    =  unitI (Num i)
interp (Add u v) e  =  interp u e `bindI` (\a ->
                       interp v e `bindI` (\b ->
                       add a b))
interp (Lam x v) e  =  unitI (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e  =  interp t e `bindI` (\f ->
                       interp u e `bindI` (\a ->
                       apply f a))

findin              :: Name -> Environment -> I Value
findin x []         =  unitI Wrong
findin x ((y,b):e)  =  if x==y then unitI b  else  findin x e

add                 :: Value -> Value -> I Value
add (Num i) (Num j) =  unitI (Num (i+j))
add a b             =  unitI Wrong

apply               :: Value -> Value -> I Value
apply (Fun k) a     =  k a
apply f a           =  unitI Wrong

test                :: Term -> String
test t              =  showI (interp t [])


-- Variation 0      : Standard Interpreter - Identity Monad
--
type  I a           =  a

unitI a             =  a
a `bindI` k         =  k a
showI a             =  showval a
