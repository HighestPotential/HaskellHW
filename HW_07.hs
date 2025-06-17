-------------1

elem' :: (Eq a) => a -> [a] -> Bool
elem' x  = foldl (\acc y -> if y==x && acc == False
                                then True
                                else acc) False


elem'r :: (Eq a) => a -> [a] -> Bool
elem'r x = foldr (\y acc -> (y==x && not acc) || acc) False

all' :: (a -> Bool) -> [a] -> Bool
all' f  = foldl (\acc x -> f x  && acc ) True

--all'' f = foldr (&& . f) True

reverse' :: [a] -> [a]
reverse'  = foldr (\x acc-> acc ++ [x]) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\x acc -> if f x then x : acc else []) []


-------------2
foo :: Num a => a -> a
foo = (*) 4

foo' :: Integer -> Integer
foo'  = (+1) . (+1)

foo2 :: b -> a -> (a, b)
foo2  = flip (,)


foo3:: Integral a => a ->Bool
foo3 = (== 1) . flip mod 2


foo4 :: (Floating a, Enum a) => Int -> a
foo4  = sqrt . (5 *). sum . flip take [1..50]
--foo4 x = sqrt (5 * (sum (take x [1..50])))


foo5 :: [Char] -> [Char]
foo5  = (++ "River") . flip (++) "Plate" . (:[]) . head

-- foo (x:_) = "River" ++ [x] ++ "Plate" 

-----------3
funa:: Integer
funa  = sum $ [x^2| x<- [1..100], odd $ x^2]
--sumOddSquares = sum $ takeWhile (< 10000) [n^2 | n <- [1,3..]]

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n | even n = n : chain (n `div` 2)
    | odd n = n : chain (n * 3 + 1)

funb::Int
funb = length $ filter (\ arr -> length arr > 15) [ chain res | res <- [1..100]]
--length legth filter [list Comprehension]

--https://claude.ai/public/artifacts/39b0150e-f083-4f05-959a-7fffdf496faa
