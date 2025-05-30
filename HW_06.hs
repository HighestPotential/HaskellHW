---1
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
    (Astronaut {benutzername = b1, farbe = f1}) == (Astronaut {benutzername = b2, farbe = f2})
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

istCrewmate:: Astronaut -> Bool
istCrewmate (Astronaut {rolle = r}) = r == Crewmate

istImpostor:: Astronaut -> Bool
istImpostor x = not (istCrewmate x)

istLebendig:: Astronaut -> Bool
istLebendig (Astronaut {zustand = z}) = z==Lebendig

istTot:: Astronaut -> Bool
istTot x = not (istLebendig x)

-------------------------------2

data ML a = E | L a (ML a) deriving Show

oneToFour :: ML Integer
oneToFour= L 1 (L 2 (L 3 (L 4 E)))
test :: ML Integer
test=      L 1 (L 2 (L 3 (L 4 (L 999 E))))

myHead::ML a-> a
myHead E = error "Empty List"
myHead (L x xs )= x

myAppend:: ML a -> ML a -> ML a
myAppend E E = E
myAppend E (L x xs) = L x (myAppend E xs)
myAppend (L x xs) b = L x (myAppend xs b)

myAdd ::Num a=> ML a -> ML a -> ML a
myAdd E x = E
myAdd x E = E
myAdd (L x xs) (L y ys) = L (y+x) (myAdd xs ys)

myString::Show a=>ML a -> String
myString E = ""
myString (L x E)= show x
myString (L x xs)= show x ++ (',': myString xs)


myLess:: Ord a => ML a -> ML a -> Bool
myLess _ E= False
myLess E _= True
myLess first second = myHead first < myHead second

-------------3
length' :: [a] -> Int
--length' arr = foldl (\ acc x -> acc+1) 0 arr 
length' = foldl (\ acc x -> acc+1) 0
