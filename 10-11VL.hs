data Peano = Zero | Succ Peano deriving (Eq, Show)


intToPeano :: Int -> Peano
intToPeano 0 = Zero
intToPeano n = Succ (intToPeano (n-1))
 
peanoAdd :: Peano -> Peano -> Peano
peanoAdd Zero y = y
peanoAdd (Succ x) y = Succ (peanoAdd x y)
