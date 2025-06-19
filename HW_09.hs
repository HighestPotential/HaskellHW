data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving Show
leaf :: a -> BinTree a
leaf x = Node x Empty Empty
--1
--a
--[6, 8, 12, 1, 3, 9, 5]
--[1,3(lroot),5,6(root),8,9(rroot),12]
binaryTreeExample :: BinTree Integer
binaryTreeExample = Node 6 (Node 3 (leaf 1)  (leaf 5))
  (Node 9 (leaf 8)  (leaf 12))

--b
leaves :: Num a => BinTree b -> a
leaves Empty = 0
leaves (Node x Empty Empty) = 1
leaves (Node x l r) = leaves l + leaves r

--c
--binarySearch
search :: Ord a => a -> BinTree a -> Bool
search toS Empty = False
search toS (Node x Empty Empty) |toS==x = True
                                |otherwise = False
search toS (Node x l r) | toS==x = True
                         | toS<x = search toS l
                         |otherwise = search toS r

-- t3 = Node 10
--            (Node 5
--                (Node 2 Empty Empty)
--                (Node 7 Empty Empty))
--            (Node 15
--                Empty
--                (Node 20 Empty Empty))
-- t4 = Node "m"
--            (Node "d"
--                (Node "a" Empty Empty)
--                (Node "f" Empty Empty))
--            (Node "z" Empty Empty)
--d
expand :: Ord a => a -> BinTree a -> BinTree a
expand x Empty = leaf x
expand x t@(Node y l r) | y > x = Node y (expand x l) r
                        | y < x = Node y l (expand x r)
                        | otherwise = t

-- t2 = Node 10
--             (Node 5 Empty Empty)
--             (Node 15 Empty Empty)
--e)
findMax :: Ord a => BinTree a -> Maybe a
findMax Empty = Nothing
findMax (Node x l r) = max (Just x)  (max (findMax l) (findMax r))

findMin :: Ord a => BinTree a -> Maybe a
findMin Empty = Nothing
findMin (Node x l r) = maybeMin (Just x)  (maybeMin (findMin l) (findMin r))
    where 
        maybeMin Nothing x = x
        maybeMin x Nothing = x
        maybeMin (Just x) (Just y) = Just (min x y)


--f)
isBST :: Ord a => BinTree a -> Bool
isBST Empty = True
isBST (Node x Empty Empty) = True
isBST (Node x l r) = Just x > findMax l && Just x < findMin r 
    && isBST l && isBST r

-- Aufgabe 2
-- a)

data Op = Plus | Minus | Times | Division
  deriving (Show, Eq)

data Negate a  = Negate (Negate a) | C a deriving (Show, Eq)

data Term a
  =  UnTerm (Negate a) 
  | BinTerm Op (Term a) (Term a)
  deriving (Show, Eq)

--b) 

eval :: Integral a => Term a -> a
eval (BinTerm Times a b)    = (*) (eval a) (eval b)
eval (BinTerm Division a b) = (div) (eval a) (eval b)
eval (BinTerm Plus a b)     = (+) (eval a) (eval b)
eval (BinTerm Minus a b)    = (-) (eval a) (eval b)
eval (UnTerm t@(Negate x) )   = helper t
    where
        helper (Negate x) = - (helper x)
        helper (C x)      = x
eval (UnTerm (C x))         = x

-- t1 :: Term Integer
-- t1=BinTerm Times (BinTerm Plus (C 5) (C 4)) (BinTerm Minus (C 3) (C 2))

-- test5 = BinTerm Division
--            (BinTerm Times
--               (BinTerm Minus (C 8) (C 3))
--               (BinTerm Plus (C 6) (C 1)))
--            (C 5)

-- --c) siehe oben

-- test5 = BinTerm Plus
--            (UnTerm (Negate (C 2)))      -- -2
--            (UnTerm (C 10))              -- +10
-- test6 = BinTerm Division
--            (BinTerm Times
--               (BinTerm Plus (UnTerm (Negate (C 3))) (UnTerm (C 6))) -- (-3 + 6) = 3
--               (BinTerm Minus (UnTerm (C 10)) (UnTerm (Negate (C 2)))) -- 10 - (-2) = 12
--            )
--            (UnTerm (C 4))
-- -- Erwartet: (3 * 12) / 4 = 36 / 4 = 9

--d)
simplify :: Term a -> Term a
simplify (BinTerm Minus (UnTerm (Negate (C a))) b)=
    BinTerm Plus (UnTerm (C a) ) b  

simplify (BinTerm Plus t@(UnTerm x) (UnTerm (Negate (C b))))=
    BinTerm Minus t (UnTerm (C b))

simplify (BinTerm Minus t@(UnTerm x) (UnTerm (Negate (C b))))=
    BinTerm Plus t (UnTerm (C b))

simplify (BinTerm x a b) = BinTerm x (simplify a) (simplify b)

simplify (UnTerm x) = UnTerm x
