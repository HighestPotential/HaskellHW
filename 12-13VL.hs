data Kinder a = Lecture a | OnlineLecture a deriving (Show)

instance Functor Kinder where
    fmap :: (a -> b) -> Kinder a -> Kinder b
    fmap f (Lecture x) = Lecture (f x)
    fmap f (OnlineLecture x) = OnlineLecture (f x)

data Kinder2 a b = Lecture2 a | OnlineLecture2 b deriving (Show)
data Shorter a = Kinder2 a Integer

data RoseTree a = Rose a [RoseTree a]

instance Foldable RoseTree where

    foldMap :: Monoid m => (a -> m) -> RoseTree a -> m
    foldMap f (Rose x []) = f x 
    foldMap f (Rose x xs) = f x <> foldMap (foldMap f) xs
 



-- data WTree a = Empty | Node (WTree a) a Int (WTree a) deriving Eq

-- instance Foldable WTree where
--     foldMap :: Monoid m => (a -> m) -> WTree a -> m
--     foldMap f Empty = mempty
--     foldMap f (Node l x w r) | w>0 = foldMap f l <> f x  <> foldMap f r
--                             | otherwise = foldMap f l <> foldMap f r



data LeafTree a = Leaf a | Fork (LeafTree a) (LeafTree a)

instance Foldable LeafTree where

    foldMap :: Monoid m => (a -> m) -> LeafTree a -> m
    foldMap f (Leaf x) = f x
    foldMap f (Fork l r) = foldMap f l <> foldMap f r 


-- data MaybeTree a = Empty | Node (Maybe a) (MaybeTree a) (MaybeTree a)

-- instance Foldable MaybeTree where
--     foldMap :: Monoid m => (a -> m) -> MaybeTree a -> m
--     foldMap f Empty = mempty
--     foldMap f (Node (Just x) l r)=  foldMap f l <> f x <> foldMap f r
--     foldMap f (Node Nothing l r)=  foldMap f l <> foldMap f r

data InnerTree a = Tip | Node a (InnerTree a) (InnerTree a)

instance Foldable InnerTree where
    foldMap f Tip = mempty
    foldMap f (Node x l r) = f x <> foldMap f l <> foldMap f r


data SnocList a = Lin | Snoc (SnocList a) a

instance Foldabel SnocList where
    foldMap f Lin = mempty
    foldMap f (Snoc list x) = foldMap f list <> f x
