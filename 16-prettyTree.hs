data Tree a = Empty | Node a (Tree a) (Tree a) deriving Functor

leaf :: a -> Tree a
leaf x = Node x Empty Empty

t :: Tree Integer
t = Node 6 (Node 13 (leaf 2) (Node 117 (leaf 56) Empty)) (Node 8 Empty (leaf 4))


instance Show a => Show (Tree a) where
    show :: Tree a -> String
    show x = let (_, _, l) = toBox x in unlines l
      where
        -- for a tree, returns (left pad to root, right pad from root, list of lines)
        toBox :: Tree a -> (Int, Int, [String])
        toBox Empty        = (1, 1, [" ε "])
        toBox (Node x Empty Empty) = let s = " " ++ padLabel x ++ " "
                                         sl = length s - 1
                                         sh = sl `div` 2 
                                         in (sh, sl - sh, [s])
        toBox (Node x l r) = let (lllen, lrlen, lbox) = toBox l
                                 (rllen, rrlen, rbox) = toBox r
                                 llen = lllen + lrlen + 1 -- add one for root
                                 rlen = rllen + rrlen + 1 -- on left and right
                                 newBox = joinLines (llen, lbox) (rlen, rbox) 
                                 label = padLabel x
                                 leftpad = llen - (length label `div` 2)
                                 rightpad = rlen - (length label `div` 2)
                                 labelline = replicate leftpad ' ' ++ label ++ replicate rightpad ' '
                                 in (llen, rlen,
                                    labelline :
                                    (replicate lllen ' ' ++ "┌" ++
                                      replicate lrlen '─' ++ "┴" ++
                                      replicate rllen '─' ++ "┐" ++
                                      replicate rrlen ' ') : 
                                    newBox)

        padLabel :: Show a => a -> String
        padLabel x = let s = show x in if odd (length s) then s else ' ' : s

        joinLines :: (Int, [String]) -> (Int, [String]) -> [String]
        joinLines (xl, x:xs) (yl, y:ys) = (x ++ " " ++ y)               : joinLines (xl, xs) (yl, ys)
        joinLines (xl, x:xs) (yl,  [] ) = (x ++ replicate (yl + 1) ' ') : joinLines (xl, xs) (yl, [])
        joinLines (xl,  [] ) (yl, y:ys) = (replicate (xl + 1) ' ' ++ y) : joinLines (xl, []) (yl, ys)
        joinLines (xl,  [] ) (yl,  [] ) = []


