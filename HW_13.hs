import Data.List
import System.IO

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

instance Default Integer where
    def :: Integer
    def =0

instance Default Bool where
    def :: Bool
    def = False

instance Default (Maybe a) where
    def :: Maybe a
    def = Nothing

instance Default [a] where
    def :: [a]
    def = []

instance (Default a) => Default (Either a b) where
    def = Left def

instance Default () where
    def = ()

instance (Default a)=>Default (a, a) where
    def :: Default a => (a, a)
    def = (def, def)

instance (Default b) => Default (a->b) where
    def = \_ ->def

failSafe :: Default a => Maybe a -> a
failSafe Nothing = def
failSafe (Just x) = x

--3
--a
functorReplace :: Functor f => a -> f b -> f a
functorReplace x elInContext = fmap (\el -> x) elInContext

--b
brokenLine :: IO ()
brokenLine  = functorReplace () getLine

--c
main' :: IO ()
main' = don't $ do
    name <- getLine
    putStrLn $ "Hello " ++ name ++ "!"

don't :: Monad m => m a -> m ()
don't consoleThings = return ()

--d
--nichtweiisen über die Monade, kann nciht genarilisiert werden, bbsp just x oder Nothing...

--4
data Team = Team {
name :: String,
nW :: Int, -- Anzahl der Siege
nD :: Int, -- Anzahl der Unentschieden
nL :: Int, -- Anzahl der Niederlagen
nGF :: Int, -- Erzielte Tore
nGA :: Int -- Kassierte Tore
} deriving (Eq, Show)

leverkusen = Team "Bayer 04 Leverkusen" 28 06 00 90 20
stuttgart = Team "VfB Stuttgart" 24 04 06 80 40
bayern = Team "Bayern Muenchen" 24 04 06 45 45
dortmund = Team "Borussia Dortmund" 20 05 09 60 25
leipzig = Team "Corporation Leipzig" 20 05 09 60 25
wolfsburg = Team "Wolfsburg" 10 20 04 50 30
berlin = Team "Union Berlin" 10 20 04 35 30
darmstadt = Team "SV Darmstadt 98" 01 10 10 15 50
bundesliga = [ bayern, berlin, darmstadt, dortmund
 ,leipzig, leverkusen, stuttgart, wolfsburg ]


--1
getpointns t1 = ((nW t1)*3)+ (nD t1)

-- instance Ord Team where
--     (<=) :: Team -> Team -> Bool
--     t1 <= t2 
--         |(points1 <= points2) =True
--         |otherwise = False

--         where 
--             points1 = getpointns t1
--             points2 = getpointns t2


instance Ord Team where
    (<=) :: Team -> Team -> Bool
    t1 <= t2
        | (name t1 == "Bayern Muenchen" && name t2 == "Borussia Dortmund") = False
        | (name t2 == "Bayern Muenchen" && name t1 == "Borussia Dortmund") = True
        | leftBigger (name t1) (name t2) = True
        | otherwise = False
      where
        leftBigger [] _ = True
        leftBigger _ [] = False
        leftBigger (x1:xs1) (x2:xs2)
            | x1 < x2 = True
            | x1 == x2 = leftBigger xs1 xs2
            | otherwise = False

--5
type KnightPos = (Int, Int)--x, y

--a
moveKnight :: KnightPos -> [KnightPos]
moveKnight (x, y) = filter (\(a,b)-> a>0 && a<=8 && b>0 && b<=8) [(x+2, y+1), (x+2, y-1), (x+1, y+2), (x-1, y+2), (x+1, y-2), (x-1, y-2), (x-2, y+1), (x-2, y-1)]

--b
in3Moves:: KnightPos -> [KnightPos]
--in3Moves pos = connect (map moveKnight (connect (map moveKnight (moveKnight pos))))
in3Moves pos = return pos >>= moveKnight >>= moveKnight >>= moveKnight

connect [] = []
connect (x:xs) = x++connect xs


--6
data Pokemon = Pokemon { pID :: Int
, pName :: String
, pHealth :: Integer
, pAttack :: Integer
, pType :: String
}

instance Show Pokemon where
    show :: Pokemon -> String
    show poke = show (pID poke) ++ ": " ++ pName poke ++
        " / HP " ++ show (pHealth poke) ++
        " / AP " ++ show (pAttack poke) ++
        " / TYPE " ++ pType poke

--pokedex.txt
pokeLoad :: FilePath -> IO [Pokemon]
pokeLoad path = do
    content <- readFile path
    let pokemonArray = map pokeParse (lines content)
    return pokemonArray


pokeParse :: String-> Pokemon
pokeParse line = Pokemon (read ((!!) separatedValues 0)) ((!!) separatedValues 1) 
    (read ((!!) separatedValues 2)) (read ((!!) separatedValues 3)) 
    ((!!) separatedValues 4)

        where separatedValues = words line


pokeBattle :: [Pokemon] -> IO ()
pokeBattle pokArray = do 
    putStrLn "Player1, choose a Pokemon (by ID)"
    id <- getLine
    let pokemon1= (pokArray !! (read id))
    putStrLn $ "(P1)" ++ " " ++ id ++ ": " ++ (show pokemon1)

    putStrLn "Player2, choose a Pokemon (by ID)"
    id2 <-getLine
    let pokemon2 = (pokArray !! (read id2))
    putStrLn $ "(P1)" ++ " " ++ id ++ ": " ++ (show pokemon1)

    putStrLn $ "Battle! " ++ (pName pokemon1) ++ " vs " ++ (pName pokemon2)
    battleLoop pokemon1 pokemon2 1


battleLoop :: Pokemon -> Pokemon -> Int -> IO ()
battleLoop pok1 pok2 round 
    |(pHealth pok1 <= 0) = do
        putStrLn $ pName pok1 ++ " fainted!"++ " " ++ pName pok2 ++ " wins!"
    |(pHealth pok2 <= 0) = do
        putStrLn $ pName pok2 ++ " fainted!"++ " " ++ pName pok1 ++ " wins!"

    |((mod round 2)==0) = do
        putStrLn $ "----- Turn " ++ (show round)
        putStrLn $ pName pok1 ++ " / "++ show (pHealth pok1)
        putStrLn $ pName pok2 ++ " / "++ show (pHealth pok2)
        putStrLn $ "Turn Turn for P2 ("++ pName pok2 ++ "attacks):"
        putStrLn $ pName pok2 ++ "attacks for "++ show (pAttack pok2) ++" damage!"
        let pok1' = pok1 { pHealth = (pHealth pok1) - (pAttack pok2) }
        battleLoop pok1' pok2 (round + 1)

    |otherwise = do
        putStrLn $ "----- Turn " ++ (show round)
        putStrLn $ pName pok1 ++ " / "++ show (pHealth pok1)
        putStrLn $ pName pok2 ++ " / "++ show (pHealth pok2)
        putStrLn $ "Turn Turn for P1 ("++ pName pok1 ++ "attacks):"
        putStrLn $ pName pok1 ++ "attacks for "++ show (pAttack pok1) ++" damage!"
        let pok2' = pok2 { pHealth = (pHealth pok2) - (pAttack pok1) }
        battleLoop pok1 pok2' (round + 1)

main :: IO ()
main = do
    pokemonsList <-pokeLoad "pokedex.txt" 
    pokeBattle pokemonsList

