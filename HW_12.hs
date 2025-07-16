--3
--a
import Control.Monad

chainAction1 :: Monad m => a -> [(a -> m a)] -> m a
chainAction1 el [] = return el
chainAction1 el (f:xs) = do
    changedEl <- f el
    chainAction1 changedEl xs

     -- TODO


chainAction2 :: Monad m => a -> [(a -> m a)] -> m a
chainAction2 el (f:xs) = let changed = f el in
    changed >>= (\takenOut -> chainAction2 takenOut xs)
chainAction2 el [] = return el

--chainaction2 a1 (h:t) = h a1 >>= (\a2 -> chainaction2 a2 t)-- alternative to first line

chainAction3 :: Monad m => a -> [(a -> m a)] -> m a
chainAction3 el (f:xs) =  undefined ------WWWWWWWWWWWWWWWWWWWWWWWWWWWWWTTTTTTTTT
--chainrecation3 = foldM (\acc f -> f acc)

tellOp :: (Show a, Show b) => (a -> b) -> a -> IO b
tellOp f x = let fx = f x in do
    putStrLn $ (show x) ++ " -> " ++ (show fx)
    return fx

test :: [Int -> IO Int]
test = map tellOp [ (*3), (+1), (`mod` 7), (+5), (*2) ]


--4
--a
data Logger a = Logger a [String]

instance (Show a) => Show (Logger a) where
    show :: Show a => Logger a -> String
    show (Logger v logs) = show v ++ "\n" ++ unlines (reverse logs)

instance Functor Logger where
    fmap :: (a -> b) -> Logger a -> Logger b -- <$>
    fmap f (Logger el strs) = Logger (f el) strs

instance Applicative Logger where
    (<*>) :: Logger (a -> b) -> Logger a -> Logger b
    (Logger f strs1) <*> (Logger x strs2) = Logger (f x) (strs1 ++ strs2)

    pure :: a -> Logger a
    pure el = Logger el []

instance Monad Logger where
    return :: a -> Logger a
    return = pure

    (>>=) :: Logger a -> (a -> Logger b) -> Logger b
    (Logger el strs) >>= f = let Logger el' strs' = f el in
        Logger el' (strs ++ strs')

--b


data Match = Match { homeTeam :: String -- Name of home team
    , awayTeam :: String -- Name of away team
    , homeScore :: Int -- Goals scored by home team
    , awayScore :: Int -- Goals scored by away team
    }

instance Show Match where
    show :: Match -> String
    show m = home ++ " - " ++ away
        where
            home = homeTeam m ++ " " ++ show (homeScore m)
            away = show (awayScore m) ++ " " ++ awayTeam m



startMatch :: String -> String -> Logger Match
startMatch h a = Logger (Match h a 0 0) ["Das Spiel beginnt"]

endMatch :: Match -> Logger Match
endMatch m = Logger m ["Spiel ist zu Ende."]

-- Given name of player (p) and minute of game (t), add one goal to home team
scoreHome :: String -> Int -> Match -> Logger Match
scoreHome p t m =  Logger (Match (homeTeam m) 
    (awayTeam m) (1+ homeScore m) (awayScore m)) [p]

-- Given name of player (p) and minute of game (t), add one goal to away team
scoreAway :: String -> Int -> Match -> Logger Match
scoreAway p t m = Logger (Match (homeTeam m) 
    (awayTeam m) (homeScore m) (1 + awayScore m)) [p]

--c
wMFinals :: Logger Match
wMFinals= do 
    startMatch "Argentinien" "Frankreich" >>= (\afterstartMatch ->
     scoreHome "Messi (ARG) trifft in der 23. Minute" 1 afterstartMatch >>= (\aftergoalMatch->
        scoreHome "Di Mar´ıa (ARG) trifft in der 36. Minute" 1 aftergoalMatch    >>= (\ aftergoal2Match -> 
            scoreAway "Mbapp´e (FRA) trifft in der 80. Minute" 1 aftergoal2Match   >>= (\ aftergoal3Match->
                scoreAway "Mbapp´e (FRA) trifft in der 81. Minute" 1 aftergoal3Match   >>= (\ aftergoal4Match->
                    scoreHome "Messi (ARG) trifft in der 108. Minute" 1 aftergoal4Match   >>= (\ aftergoal5Match->
                        scoreAway "Mbapp´e (FRA) trifft in der 118. Minute" 1 aftergoal5Match >>= (\ aftergoal6Match ->
                            endMatch aftergoal6Match))))) ))

printLogger :: Show a => Logger a -> IO ()
printLogger (Logger match notes)= do
    print match
    print notes

doit :: IO ()
doit= printLogger wMFinals
