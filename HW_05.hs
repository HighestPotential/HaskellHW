--1
--[9, 12, 18] :: Num a => [a]
--"che" ++ ['e', 's', 'e'] ::[Char]
-- [1 + 1, 2.0 * 1, 3 - 1]:: Fractional a => [a]
--[("a" : "bba", 1972), ("slip" ++ "knot", 1995)]-- ERROR Cons with 2 Strings
--(2000, "Bernd") == (1.1, []) :: Bool
--[1930, 1950, [1978, 1986, [2022]]]:: (Num a, Num [a], Num [[a]]) => [[[a]]]
--Error array inhomogen
--[[[1], [2] ++ [3]], [], [[5], [6]]] :: Num a => [[[a]]]
--() /= (10, 'M')-> Fehler (unterschiedliche Typen)

------2
--f::Integral a =>a->a -- da Integral erbt von Eq 
--f 0 = 1
--f n = n * f (n - 1)


--f::Integral a => [a]->[a] ----mod relevant für Klausur, Integral
-- f x = [y | y <- x, y `mod` 2 == 0]

--f::(Ord x, Eq y) =>x->y->()->() -----succ :: Enum a => a -> a
--f :: (Ord a, Enum a) => a -> a -> () -> ()
--f x y z = if succ x < y then z else ()

--f::(Read x, Show y)=>String->y->(x,String)
-- f x y = (read x, show y)

---3
data DomFoot = LeftF | RightF deriving (Show, Eq)
data PPosition = Goalkeeper | Defender | Midfielder | Forward deriving (Show, Eq)

data Player = Constr String String Integer DomFoot PPosition deriving (Show, Eq)

p1 = Constr "Messi" "FC Barcelona" 11 LeftF Forward
p2 = Constr "Ronaldo" "FC Barcelona" 121 LeftF Midfielder
p3 = Constr "Suarez" "FC Barcelona" 1231 LeftF Midfielder
p4 = Constr "Pepe" "FC Barcelona" 111 LeftF Goalkeeper
p5 = Constr "Pepe" "FC Barcelona" 1191 LeftF Defender
team=[p1, p2, p3, p4, p5]

---4
getClub::Player->String
getClub (Constr _ club _ _ _) = club

getNum::Player->Integer
getNum (Constr _ _ backNum _ _) = backNum

getPos::Player->PPosition
getPos (Constr _ _ _ _ pos) = pos

sameTeam :: [Player] -> Bool
sameTeam arr@(x:xs) = length [y| y <-arr, toCompare == getClub y] 
    == length arr
    where
        toCompare = getClub x



uniqueNumbers :: [Player] -> Bool
uniqueNumbers arr= (length . getNumbers) arr ==
     (length . getUniqueNumbers . getNumbers) arr

getNumbers::[Player]->[Integer]
getNumbers = map getNum

getUniqueNumbers :: [Integer]->[Integer]
getUniqueNumbers []=[]
getUniqueNumbers (x:xs) = x:getUniqueNumbers (filter (/=x) xs)




coverPositions :: [Player] -> Bool
coverPositions arr = searchForCovered positionToCover currentPostions  
    where 
        currentPostions = getCurrentPostions arr
        positionToCover::[PPosition]
        positionToCover = [Goalkeeper, Defender, Midfielder, Forward]

        getCurrentPostions:: [Player]->[PPosition]
        getCurrentPostions = map getPos

        searchForCovered::[PPosition] ->[PPosition] ->Bool
        searchForCovered [] _ = True
        searchForCovered (x:xs) currPostions = doesExist x currPostions && searchForCovered xs currPostions



doesExist::PPosition->[PPosition]->Bool
doesExist _ [] = False
doesExist pos (x:xs) = x==pos || doesExist pos xs 



main :: IO ()
main = do
    putStrLn "== Test: getClub =="
    print $ getClub p1 == "FC Barcelona"

    putStrLn "\n== Test: getNum =="
    print $ getNum p2 == 121

    putStrLn "\n== Test: getPos =="
    print $ getPos p5 == Defender

    putStrLn "\n== Test: sameTeam =="
    print $ sameTeam team == True
    print $ sameTeam [p1, Constr "X" "PSG" 10 LeftF Forward] == False

    putStrLn "\n== Test: getNumbers =="
    print $ getNumbers team == [11,121,1231,111,1191]

    putStrLn "\n== Test: getUniqueNumbers =="
    print $ getUniqueNumbers [1,2,1,3,2] == [1,2,3]
    print $ getUniqueNumbers [5,5,5,5] == [5]

    putStrLn "\n== Test: uniqueNumbers =="
    print $ uniqueNumbers team == True
    print $ uniqueNumbers (team ++ [p1]) == False  -- duplicate p1's number (11)

    putStrLn "\n== Test: coverPositions =="
    print $ coverPositions team == True
    print $ coverPositions [p1, p2, p3] == False  -- no Goalkeeper or Defender

    putStrLn "\n== Test: doesExist =="
    print $ doesExist Midfielder [Goalkeeper, Midfielder] == True
    print $ doesExist Forward [Defender, Goalkeeper] == False
