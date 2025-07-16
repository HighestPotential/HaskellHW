
elem' :: (Eq a) => a -> [a] -> Bool
elem' x ar = 
    foldl (\acc el-> if el == x && acc == False || acc == True then True else False ) False ar

reverse' :: [a] -> [a]
reverse' ar = foldl (\ acc el  ->el : acc  ) [] ar

foo :: b -> a -> (a, b)
foo = flip (,)

foo2 :: Integral a => a -> Bool

foo2  = (==) 1 . flip mod 2


foo3  = sqrt . ((*) 5) . (sum) . flip take [1..50]


foo100 = length . take 10


foo8 = head . (drop 5) . tail



-- length' (x:xs) = 1+ length' xs
-- length' [] = 0

length' ar = helper ar 0
    where
        helper (x:xs) acc = helper xs (acc+1)
        helper [] acc = acc

take' 0 _ = []
take' amt (x:xs) = x: (take' (amt - 1) xs)
take' amt [] = []


take'' amt ar= helper amt ar []
    where 
        helper 0 _ acc = acc
        helper amt (x:xs) acc = helper (amt-1) xs (x:acc)
        helper amt [] acc = acc
