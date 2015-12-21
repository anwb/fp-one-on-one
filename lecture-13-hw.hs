-- Exercise 0
--
_foldl1 f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a
_foldl2 f a bs = foldr (\a b -> f b a) a bs
_foldl3 f      = flip $ foldr (\a b g -> b (f g a)) id
_foldl4        = foldr . flip