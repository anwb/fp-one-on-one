dropwr p = foldl add []
  where
    add [] x  = if p x then [] else [x]
    add acc x = x : acc

sumsqreven = compose [map (^ 2), filter even]
-- sumsqreven = compose [sum, map (^ 2), filter even]
{-
The above doesn't typecheck as sum is not of type: a -> a
sum         :: Num t    => [t] -> t
map (^2)    :: Num t    => [t] -> [t]
filter even :: Integral => [t] -> [t]
-}

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x        = []
  | otherwise  = h x : unfold p h t (t x)

iterateviaunfold :: (a -> a) -> a -> [a]
iterateviaunfold f = unfold (const False) id f

