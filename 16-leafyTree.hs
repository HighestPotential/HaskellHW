data Tree a = Leaf a | Branch (Tree a) (Tree a)

nums1 :: Tree Integer
nums1 = Branch (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)) (Branch (Leaf 4) (Leaf 5))

nums2 :: Tree Integer
nums2 = Branch (Leaf 10) (Leaf 8)

instance Show a => Show (Tree a) where
    show :: Tree a -> String
    show x = let (_, _, l) = toBox x in unlines l
      where
        -- for a tree, returns (left pad to root, right pad from root, list of lines)
        toBox :: Tree a -> (Int, Int, [String])
        toBox (Leaf x)     = let s = " " ++ show x ++ " "
                                 sl = length s - 1   -- -1 to match inner nodes, which have a root
                                 sh = sl `div` 2 in  -- padding should be half the length
                                 (sh, sl - sh, [s])
        toBox (Branch l r) = let (lllen, lrlen, lbox) = toBox l
                                 (rllen, rrlen, rbox) = toBox r
                                 llen = lllen + lrlen + 1 -- add one for root
                                 rlen = rllen + rrlen + 1 -- on left and right
                                 newBox = joinLines (llen, lbox) (rlen, rbox) in
                                 (llen, rlen,
                                   (replicate lllen ' ' ++ "┌" ++
                                    replicate lrlen '─' ++ "┴" ++
                                    replicate rllen '─' ++ "┐" ++
                                    replicate rrlen ' ') : newBox)

        joinLines :: (Int, [String]) -> (Int, [String]) -> [String]
        joinLines (xl, x:xs) (yl, y:ys) = (x ++ " " ++ y)               : joinLines (xl, xs) (yl, ys)
        joinLines (xl, x:xs) (yl,  [] ) = (x ++ replicate (yl + 1) ' ') : joinLines (xl, xs) (yl, [])
        joinLines (xl,  [] ) (yl, y:ys) = (replicate (xl + 1) ' ' ++ y) : joinLines (xl, []) (yl, ys)
        joinLines (xl,  [] ) (yl,  [] ) = []

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Leaf x)     = Leaf (f x)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- Terminationsbeweis für fmap:
-- Betrachte g = fmap f; 
--   g (Leaf x)     = Leaf (f x)
--   g (Branch l r) = Branch (g l) (g r)
-- Dann gilt g :: Tree a -> Tree b, d.h. Menge A = Tree a
-- wähle A' = Tree B' so, dass f x für alle x in B' definiert ist.
-- Zu zeigen:
--   Abgeschlossenheit:
--     Zu zeigen: solange t in A' wird auch g rekursiv nur 
--     mit Argumenten aus A' angewendet.
--     Es gibt nur zwei rekursive Aufrufe mit den Teilbäumen 
--     von t. Wenn t in A' ist, sind auch seine beiden Teilbäume 
--     in A', da die Einschränkung von B' für alle Markierungen 
--     gelten muss.)
--   Definiertheit:
--     Zu zeigen: Wenn alle Anwendungen von g definiert sind, 
--     ist der gesamte Rumpf von g definiert
--     Zwei Fälle im Rumpf zu unterscheiden:
--       Leaf: Konstruktor ist überall definiert; f ist 
--             definiert durch Wahl von A' bzw. B'.
--       Branch: Rekursive Aufrufe von g sind nach Annahme 
--             definiert, Konstruktor Branch ist überall 
--             definiert.
--   Abstiegsfunktion:
--     m : hoehe
--     hoehe ist mindestens 0 und daher wohlfundiert
--     g wird rekursiv nur mit Teilbäumen von t aufgerufen, 
--     die per Definition eine Höhe von höchstens (t - 1) haben.
--     (Höhe ließe sich auch vermeiden z.B. mit Anzahl Konstruktoren)
--   Daher terminiert fmap

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f (Leaf x) = f x
    foldMap f (Branch l r) = foldMap f l <> foldMap f r

-- examples for foldMap: 
-- foldMap (Product) nums1
-- foldMap (Sum) nums1
-- foldMap (:[]) nums1

foldExample :: [Char]
foldExample = foldr (\n s -> show n ++ s) "" nums1

-- (++) :: [a] -> [a] -> [a]
-- show :: Show a => a -> String
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (.) :: (String -> (String -> String)) -> (Integer -> String) -> Integer -> (String -> String)
foldPointFree :: [Char]
foldPointFree = foldr ((++) . show) "" nums1
