module Ch20 where
-- Foldable
-- A foldable structure has a way to reduce the values inside to one summary
-- value by recursively applying some function. This always depends on some
-- Monoid instance.
import Data.Monoid
import Data.Foldable

-- While the foldr function requires an initial accumulator and a function
-- (a -> b -> b), the fold function requires a monoid and uses <> and mempty.
six = foldr (+) 0 [1..5]
sumSix = fold [Sum 1, Sum 2, Sum 3]
prod120 = fold [1, 2, 3, 4, 5 :: Product Integer]

-- Foldmap foldable structure and a function that converts that structure's elements
-- into a monoid type:
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- A minimal Foldable instance defines either foldr or foldMap

data Identity a = Identity a
    deriving (Eq, Show)

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

productFiveHundred = foldMap (*5) (Identity 100) :: Product Integer
five = foldr (*) 1 (Identity 5)

-- Maybe
data Optional a = Nada | Yep a
    deriving (Eq, Show)

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

seven = foldr (+) 2 (Yep 5)
two = foldr (+) 2 Nada

-- Other useful, derived functions from Foldable
-- toList :: Foldable t => t a -> [a]
lstlst = map toList [Just 1, Just 2, Just 3]
lst = concatMap toList [Just 1, Just 2, Just 3]

-- null :: Foldable t => t a -> Bool
-- Returns true on left / Nothing values
nullEx = fmap null [Just 1, Just 2, Nothing]

-- length :: Foldable t => t a -> Int
l1 = length (1, 2)
l3 = length [(1, 2), (3, 4), (5,6)]
lst1 = fmap length [(1, 2), (3, 4), (5, 6)]

one = fmap length Just [1..3]
ones = fmap length [Just 1, Just 2, Just 3]
onesAndZero = fmap length [Just 1, Just 2, Nothing]

-- elem :: Eq a, Foldable t => a -> t a -> Bool
-- The key to remember here is that we can only fold over the a
-- type that is part of the structure.  So in (Either b a), we can't
-- look at the b, even if it would have matched:
falsy = elem 2 (Left 2)
truthy = elem 2 (Right 2)

-- maximum :: (Ord a, Foldable t) => t a -> a
-- minimum :: (Ord a, Foldable t) => t a -> a
just10 = fmap maximum (Just [3, 7, 10, 2])

-- sum :: (Foldable t, Num a) => t a -> a
-- product :: (Foldable t, Num a) => t a -> a
twelve = sum (7,12)
fiveFour = fmap sum [(7, 5), (3, 4)]
just120 = fmap product (Just [1..5])

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
--elem' x = getAny . foldMap Any . fmap (== x) . toList
elem' x = getAny . foldMap (\y -> Any (y == x))
--elem' = any . (==)
-- ^ All of these implementations work, with the final one being the real implementation.

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

-- Defining fold in terms of foldMap
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- Defining foldMap in terms of foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> f a <> b) mempty

data Two a b = Two a b
    deriving (Eq, Show)

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Foldable (Three' a) where
    foldMap f (Three' a b b') = f b <> f b'

-- Filter function for Foldable types using foldMap
filterF :: (Applicative f, Foldable t, Monoid (f a))
            => (a -> Bool) -> t a -> f a
filterF p = foldMap (\x -> if p x then pure x else mempty)
