-- The Essence of Functional Programming
-- P. Wadler, 1992
-- 

-- Test data
-- 
-- ((\x.x+x)(10+11))
--
expr = (App (Lam "x" (Add (Var "x") (Var "x")))(Add (Con 10) (Con 11)))


type  Name            =  String

type  Position        =  Int

data  Term            =  Var Name
                      |  Con Int
                      |  Add Term Term
                      |  Lam Name Term
                      |  App Term Term
                      |  At  Position Term

data Value            =  Wrong
                      |  Num Int
                      |  Fun (Value -> P Value)

type  Environment     =  [(Name, Value)]

showval               :: Value -> String
showval Wrong         =  "<wrong>"
showval (Num i)       =  show i
showval (Fun f)       =  "<function>"

interp                :: Term -> Environment -> P Value
interp (Var x) e      =  findin x e
interp (Con i) e      =  unitP (Num i)
interp (Add u v) e    =  interp u e `bindP` (\a ->
                         interp v e `bindP` (\b ->
                         add a b))
interp (Lam x v) e    =  unitP (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e    =  interp t e `bindP` (\f ->
                         interp u e `bindP` (\a ->
                         apply f a))
interp (At p t) e     =  resetP p (interp t e)

findin                :: Name -> Environment -> P Value
findin x []           =  errorP ("unbound variable: " ++ x)
findin x ((y,b):e)    =  if x==y then unitP b  else  findin x e

add                   :: Value -> Value -> P Value
add (Num i) (Num j)   =  unitP (Num (i+j))
add a b               =  errorP ("should be numbers: " ++ showval a
                                               ++ ", " ++ showval b)

apply                 :: Value -> Value -> P Value
apply (Fun k) a       =  k a
apply f a             =  errorP ("should be function: " ++ showval f)

test                  :: Term -> String
test t                =  showP (interp t []) 0


-- Variation 1        :  Error Messages - Error Monad
--
data   E a            =  Success a | Error String

unitE  a              =  Success a
errorE s              =  Error s

(Success a) `bindE` k =  k a
(Error s)   `bindE` k =  Error s

showE (Success a)     =  "Success: " ++ showval a
showE (Error s)       =  "Error: " ++ s


-- Variation 2        :  Error Messages - With Position
--
type  P a             =  Position -> E a

unitP a               =  \p -> unitE a
errorP s              =  \p -> errorE (showval p ++ ": " ++ s)

m `bindP` k           =  \p -> m p `bindE` (\x -> k x p)

showP m pos0          =  showE (m pos0)

resetP                :: Position -> P a -> P a
resetP q m            =  \p -> m q
