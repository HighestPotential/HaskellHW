--1
succ' :: Integer -> Integer
succ' x = x + 1
pred' :: Integer -> Integer
pred' x = x - 1
opp' :: Integer -> Integer
opp' x = - x

--a
plus :: Integer -> Integer -> Integer
plus 0 y = y
plus x y
        |(x >= 0 && y>=0) = plus (pred' x) (succ' y)
        |(x<0 && y<0) = opp' (plus (opp' x) (opp' y))
        |(x<0) =  plus (pred' y) (succ' x)   -- (-x) + y= y + (-x)
        | otherwise =  plus (pred' x) (succ' y)      -- x + (-y)


minus :: Integer -> Integer -> Integer -- x - y
minus x 0 = x
minus x y
        |(y>=0) = minus (pred' x) (pred' y)
        |(y<0) = minus (succ' x) (succ' y) -- (-x) - (-y)

--b
mult ::Integer -> Integer -> Integer
mult _ 0 = 0
mult 0 _= 0
mult x y
    |(y<0) = mult (opp' x) (opp' y)
    |otherwise =helper x y 0
        where
            helper x 0 acc = acc
            helper x y acc = helper x (pred' y) (plus acc x)

fact :: Integer -> Integer
fact 0 = 1
fact 1= 1
fact (-1) = -1
fact x = helper x 1
    where
        helper (-1) acc = acc
        helper 1 acc = acc
        helper x acc | (x < 0) = helper (succ' x) (mult acc x)
                |otherwise = helper (pred' x) (mult acc x)

--2
class Default a where
    def::a
