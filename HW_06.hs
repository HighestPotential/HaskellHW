-------------1---Klausurrelevant:
data Astronaut = Astronaut {benutzername :: String, rolle::Rolle,
 zustand::Zustand, aufgaben::[String], farbe::Farben} deriving (Ord)

data Rolle = Crewmate | Impostor deriving (Eq, Ord, Enum, Show)

data Zustand = Tot | Lebendig deriving (Eq, Ord, Enum, Show)

data Farben =  Blau | Rot | Lila | Gelb | Rosa deriving (Ord, Enum)

listeFarben::[Farben]
listeFarben = [Blau, Rot, Lila, Gelb, Rosa]

instance Eq Farben where
    (==)::Farben->Farben->Bool
    Blau == Blau = True
    Rot == Rot = True
    Lila == Lila = True
    Gelb == Gelb = True
    Rosa == Rosa = True
    _    == _    = False

instance Eq Astronaut where
    (==):: Astronaut->Astronaut->Bool
    (Astronaut {benutzername = b1, farbe = f1}) ==
         (Astronaut {benutzername = b2, farbe = f2})
     = b1 == b2 && f2 ==f1

instance Show Farben where
    show :: Farben -> String
    show Blau ="Blau"
    show Rot ="Rot"
    show Lila ="Lila"
    show Gelb ="Gelb"
    show Rosa ="Rosa"

instance Show Astronaut where
    show :: Astronaut -> String
    show (Astronaut {benutzername = b, farbe = f}) = b ++ ( ':' : show f)
--show a = benutzername a ++ (':' show (farbe a))
istCrewmate:: Astronaut -> Bool
istCrewmate (Astronaut {rolle = r}) = r == Crewmate
--istCrewmate = (crewmate ==) . rolle  
istImpostor:: Astronaut -> Bool
istImpostor x = not (istCrewmate x)
--istImpostor= not . istCrewmate

istLebendig:: Astronaut -> Bool
istLebendig (Astronaut {zustand = z}) = z==Lebendig

istTot:: Astronaut -> Bool
istTot x = not (istLebendig x)

-------------------------------2

data ML a = E | L a (ML a) deriving Show

oneToFour :: ML Integer
oneToFour= L 1 (L 2 (L 3 (L 4 E)))-- klausurrelevant
test :: ML Integer
test=      L 1 (L 2 (L 3 (L 4 (L 999 E))))

myHead::ML a-> a
myHead E = error "Empty List"
myHead (L x _ )= x

myAppend:: ML a -> ML a -> ML a
myAppend E ys = ys
myAppend (L x xs) b = L x (myAppend xs b)

myAdd ::Num a=> ML a -> ML a -> ML a
myAdd E _ = E
myAdd _ E = E
myAdd (L x xs) (L y ys) = L (y+x) (myAdd xs ys)

myString::Show a=>ML a -> String
myString E = ""
myString (L x E) = show x
myString (L x xs)= show x ++ (',': myString xs)


myLess:: Ord a => ML a -> ML a -> Bool
myLess _ E= False
myLess E _= True
myLess first second = myHead first < myHead second

--or
myLess2 :: Ord a => ML a -> ML a -> Bool
myLess2 _ E = False
myLess2 E _ = True
myLess2 (L x xs) (L y ys) | x<y = True
                        | x>y = False
                        | otherwise = xs `myLess2` ys
-----------------3----Kommt in der Klausur!!!!!!!!10000%
--foldr efficienter; foldl kann auf unendliche Listen angewendet werden
length' :: [a] -> Int
--length' arr = foldl (\ acc x -> acc+1) 0 arr 
length' = foldl (\ acc x -> acc+1) 0

any' :: (a -> Bool) -> [a] -> Bool
any' f = foldl (\acc x-> f x || acc) False
--oder any'' f = foldr ( (||) . f) False

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty List"
maximum' arr = foldl (\acc x -> if x>acc then x else acc) (head arr) arr

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Empty List"
maximum'' arr = foldr (\x acc-> if x>acc then x else acc) (head arr) arr

unzip':: [(a, b)] -> ([a], [b])
unzip' = foldl (\(x,y) (f,s) -> (x++[f], y++[s])) ([],[]) --first is acc and next is x
--unzip' tArr = foldl (\(x,y) (f,s) -> (x++[f], y++[s])) ([],[]) tArr
--unzip' tArr = foldr (\ (f,s) (x,y) -> (f:x, s:y)) ([],[]) tArr