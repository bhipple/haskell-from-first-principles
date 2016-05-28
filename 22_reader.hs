{-# LANGUAGE InstanceSigs #-}
module Ch22 where
import Control.Applicative
import Control.Monad.Reader
import Data.Char
import Data.Maybe
import Data.Monoid
-- The Reader Monad addresses the singleton problem: we have some data that's
-- needed basically everywhere, and we don't want to pass it down and pollute
-- the type signatures of all our functions along the way.

hurr = (*2)
durr = (+10)

m :: Integer -> Integer
m = hurr . durr

-- This is equivalent: the functor of functions is function composition!
m' :: Integer -> Integer
m' = fmap hurr durr
-- Here, the "functor" context is a partially applied function. fmap here
-- lifts the one partially applied function over the next.

-- Here, the argument is passed to hurr and durr in parallel and the results
-- are added together.
m2 :: Integer -> Integer
m2 = (+) <$> hurr <*> durr
-- Roughly:  ((+) <$> (*2) <*> (+10)) x

m3 :: Integer -> Integer
m3 = liftA2 (+) hurr durr

hurrDurr = do
    a <- hurr
    b <- durr
    return (a + b)

-- The fundamental idea of a Reader is that it's a way of stringing functions
-- together when all those functions are awaiting one input from a shared
-- environment.

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

-- The actual Reader Monad is a newtype wrapper of the function type:
newtype Reader' r a = Reader' { runReader' :: r -> a }

instance Functor (Reader' r) where
    fmap :: (a -> b) -> Reader' r a -> Reader' r b
    fmap f (Reader' ra) = Reader' $ \r -> f (ra r)

ask :: Reader' a a
ask = Reader' id

newtype HumanName = HumanName String
    deriving (Eq, Show)

newtype DogName = DogName String
    deriving (Eq, Show)

newtype Address = Address String
    deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName,
    dogName :: DogName,
    address :: Address
    } deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName,
    dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- Without a Reader, we have to do this:
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- With the function Reader technique, we can leverage applicative:
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- Equivalently
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address

-- Exercises
asks :: (r -> a) -> Reader' r a
asks f = Reader' $ \x -> f x

instance Applicative (Reader' r) where
    -- Constructs a function that takes a value of type r and returns a value of
    -- type a. Given we know nothing about r, we have to just throw it away :)
    pure :: a -> Reader' r a
    pure a = Reader' $ const a

    -- We apply the r argument to (r -> a -> b) to get a function (a -> b),
    -- and we apply the r argument to (r -> a) to get an a, and then we
    -- apply that result to get a b.  The end result being a (r -> b)
    (<*>) :: Reader' r (a -> b) -> Reader' r a -> Reader' r b
    (Reader' rab) <*> (Reader' ra) = Reader' $ \r -> rab r (ra r)

-- TODO: pg 801
getDogR'' :: Reader' Person Dog
getDogR'' = Reader' $ Dog <$> dogName <*> address

-- Using the function monad
getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

instance Monad (Reader' r) where
    return = pure
    (>>=) :: Reader' r a -> (a -> Reader' r b) -> Reader' r b
    (Reader' ra) >>= aRb = Reader' $ \r -> runReader' (aRb (ra r)) r

getDogRM' :: Reader' Person Dog
getDogRM' = Reader' $ Dog <$> dogName <*> address

--  ===========================================================================
--                          Chapter Exercises
--  ===========================================================================
x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

xs = lookup 3 $ zip x y
ys = lookup 6 $ zip y z
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

bolt :: Integer -> Bool
bolt x = (x > 3) && (x < 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

demo :: IO ()
demo = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequA 7

demo2 :: IO ()
demo2 = do
    print . mconcat $ All <$> sequA 7
    print . mconcat $ All <$> sequA 6
    print . sequA $ fromMaybe 0 s'
    print . bolt $ fromMaybe 0 ys
    print . bolt . fromMaybe 0 $ z' 3
    print . bolt . fromMaybe 0 $ z' 1
