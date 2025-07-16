--1
--a
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
data Liste a = Nil | Cons a (Liste a) deriving (Show)

--b

instance Functor Liste where

    fmap :: (a -> b) -> Liste a -> Liste b
    fmap f Nil = Nil
    fmap f (Cons x xs)= Cons (f x) (fmap f xs)


negateFunctor :: (Functor f, Num b) => f b -> f b
negateFunctor xs = fmap negate xs

--c
instance Applicative Liste where
    (<*>) :: Liste (a -> b) -> Liste a -> Liste b
    Cons f xs <*> Cons el ms= Cons (f el) (xs <*> ms)
    _ <*> _ = Nil

    pure :: a -> Liste a
    pure el = Cons el (pure el)


--2
--a
type Money = Int
type Debit = Money
type Credit = Money
type Account = (Debit, Credit)  --shulden, guthaben

--b
withdraw :: Money -> Account -> Maybe Account
withdraw amount (shulden, guthaben)
    |(shulden+amount) <= guthaben = Just (shulden + amount , guthaben)
    |amount<0 = Nothing
    |otherwise = Nothing

deposit :: Money -> Account -> Maybe Account
deposit amount (shulden, guthaben)
    |amount<0 = Nothing
    |otherwise= Just (shulden , guthaben+amount)

--c
doTests :: Account -> Maybe Account
doTests acount = do
    afterTest1 <- deposit 150 acount
    afterTest2 <- withdraw 100 afterTest1
    afterTest3 <- withdraw 40 afterTest2
    afterTest4 <- withdraw 15 afterTest3
    afterTest5 <- deposit 30 afterTest4
    return afterTest5

doTests2 :: Account -> Maybe Account
doTests2 acount = do
    afterTest1 <- deposit 20 acount
    afterTest2 <- withdraw 15 afterTest1
    afterTest3 <- deposit 30 afterTest2
    afterTest4 <- deposit 20 afterTest3
    afterTest5 <- deposit 10 afterTest4
    return afterTest5


--d
doTests' :: Account -> Maybe Account
doTests' acount = (deposit 150 acount) >>= (\ac1 ->
    (withdraw 100 ac1) >>= (\ac2 ->
        (withdraw 40 ac2) >>= (\ac3 ->
            (withdraw 15 ac3) >>= (\ac4 ->
                (deposit 30 ac4)))))

--e
type Balance = Money

accountState :: Account -> Maybe Balance
accountState (shulden, guthaben)
    | shulden > guthaben = Nothing
    | otherwise = pure (guthaben-shulden) -- oder statt pure einfach Just


unpure :: (Num a, Num b) => Maybe (a, b) -> (a, b)
unpure (Just (x,y))= (x,y)
unpure _= (-10000000000000000000,-100000000000000)

--3
--a
data Box a = Empty String | Full a deriving Show

--b
instance Functor Box where
    fmap :: (a -> b) -> Box a -> Box b
    fmap f (Full a) = Full (f a)
    fmap f (Empty msg) = Empty msg

--c
instance Applicative Box where
  (<*>) :: Box (a -> b) -> Box a -> Box b
  (Full f) <*> (Full x) = Full (f x)
  (Empty msg) <*> _ = Empty msg
  _ <*> (Empty msg) = Empty msg

  pure :: a -> Box a
  pure = Full

--d
instance Monad Box where
    (>>=) :: Box a -> (a -> Box b) -> Box b
    (Full el) >>= f = f el
    (Empty msg) >>= f = Empty msg

    return :: a -> Box a
    return=pure
--Ask about the  "(>>=) :: Box a -> (a -> Box b) -> Box b" will they be asked in Klausur???
--Singnatur