f :: Integer -> Integer -> Integer
f x y = (x * 5 + 10) `div` y

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (Integer -> (Integer -> Integer)) -> (Integer -> Integer) -> Integer -> (Integer -> Integer)
f' :: Integer -> Integer -> Integer
f' = div . (+10) . (*5)

g :: Integer -> Integer -> Integer
g x y = x * y + 10

g' :: Integer -> Integer -> Integer
g' x = (+10) . (x*)

-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (Integer -> Integer) -> (Integer -> (Integer -> Integer)) -> a -> c
g'' :: Integer -> Integer -> Integer
g'' = (+10) ... (*)
-- g'' = (+10) . (*)    doesn't work

-- Custom function composition for functions with two arguments 
-- followed by a function with a single argument
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f ... g = \x y -> f (g x y)

