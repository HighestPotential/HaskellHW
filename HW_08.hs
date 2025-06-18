
import Data.Semigroup
import Language.Haskell.TH (Lit(IntegerL))

-- Aufgabe 1
-- a)

data ComplexNumbers = C (Double, Double)

instance Semigroup ComplexNumbers where
        (<>) :: ComplexNumbers -> ComplexNumbers -> ComplexNumbers
        C (a, b) <> C (x, y) = C (a*x - b*y, b*x + a*y)


instance Monoid ComplexNumbers where
  mempty :: ComplexNumbers
  mempty = C (1, 0)  -- neutralelement
  mappend :: ComplexNumbers -> ComplexNumbers -> ComplexNumbers
  mappend = (<>)  -- Kommentar: Wir binden mappend an (<>), so bleiben wir konsistent.

instance Show ComplexNumbers where
    show :: ComplexNumbers -> String
    show  (C (x, y)) = show x ++ signChar ++ show (abs y) ++ "i"
        where signChar = if y<0 then " - " else " + "



-- b)
type Red = Integer
type Green = Integer
type Blue = Integer

data RGB = RGB (Red, Green, Blue) deriving (Show)

rgbAdd :: (Ord a, Num a) => a -> a -> a
rgbAdd a b | (a+b)>255 = 255
            | otherwise = a+b

instance Semigroup RGB where
    (<>) :: RGB -> RGB -> RGB
    RGB (a, b, c) <> RGB (x, y, z)  = RGB (rgbAdd a x, rgbAdd b y, rgbAdd c z)

instance Monoid RGB where
    mappend :: RGB -> RGB -> RGB
    mappend = (<>)
    mempty :: RGB
    mempty = RGB (0,0,0)

-- subtraktiven Farbmischung? -> Nein, da Operation nicht assoziativ
--1 - (2 - 3) = 2 ungleich -4 = (1-2)-3 

--Aufgabe 2
-- a)
op :: Int -> Int -> Int
op 0 0 = 1
op 1 1 = 1
op _ _ = 0

--b)
{-Es erfüllt die assoziativität und hat ein neutrales element, er ist: 1
0 x 1 =0
1x0=0
1x1=1

Assoziativität:
Sei a, b, c element {0,1} 
a # b # c()-}

--c)
newtype XOR = XOR Int deriving Show

instance Semigroup XOR where
    (<>) :: XOR -> XOR -> XOR
    XOR 1 <> XOR 1 =XOR 1--oder einfach (<>)= op
    XOR 0 <> XOR 0 =XOR 1
    XOR _ <> XOR _ =XOR 0

instance Monoid XOR where
    mappend :: XOR -> XOR -> XOR
    mappend = (<>) --wird automatisch gemacht!!
    mempty :: XOR
    mempty = XOR 1
    
-- Aufgabe 3
--a
data Triple a = Triple a a a deriving (Eq)

instance Show a => Show (Triple a) where 
    show :: Show a => Triple a -> String
    show (Triple x y z)= '(': show x ++ ", "++ show y ++ ", " ++ show z ++")"

--b

tfst :: Triple a -> a
tfst (Triple x _ _) = x

tsnd :: Triple a -> a
tsnd (Triple _ x _) = x

ttrd :: Triple a -> a
ttrd (Triple _ _ x ) = x

--c

x :: Num a => Triple a -> Triple a -> Triple a
x (Triple a b c) (Triple d e f) = Triple (b*f - c*e) (c*d - a*f) (a*e -b*d)  

--d
instance Functor Triple where
    fmap :: (a -> b) -> Triple a -> Triple b
    fmap f (Triple a b c) = Triple (f a) (f b) (f c)

scaMult :: Num a => a -> Triple a -> Triple a
scaMult s = fmap (* s) 
--scaMult s triple = fmap (* s) triple
--scaMult = fmap . (*)