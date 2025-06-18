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
search toS (Node x l r) | toS==x = True
                         | toS<x = search toS  
