data Kinder a = Lecture a | OnlineLecture a deriving (Show)

instance Functor Kinder where
    fmap :: (a -> b) -> Kinder a -> Kinder b
    fmap f (Lecture x) = Lecture (f x)
    fmap f (OnlineLecture x) = OnlineLecture (f x)

data Kinder2 a b = Lecture2 a | OnlineLecture2 b deriving (Show)
data Shorter a = Kinder2 a Integer
