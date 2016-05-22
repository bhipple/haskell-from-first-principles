module Ch18 where
import Control.Monad (join, liftM2)
import Control.Applicative ((<**>))

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
-- Monads
-- Applicative Functors with bind and sequencing functions
-- The generalization here is that we can do mappings that generate additional
-- layers of structure, but we can also 'concat' that structure back down to
-- just the original layer.
--
-- The "secret sauce" of Monads is the join function, which lets us strip
-- away a layer of structure:
-- join :: Monad m => m (m a) -> m a
bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m

andOne :: Int -> [Int]
andOne x = [x, 1]

ex1 :: [[Int]]
ex1 = fmap andOne [4..6]

ex2 :: [Int]
ex2 = bind andOne [4..6]

-- Consider this:
h :: IO (IO ())
h = putStrLn <$> getLine
-- The printing never gets executed, because the inner IO is lazily evaluated!

twoActions = (putStrLn "1", putStrLn "2")
-- We can take fst / snd of this to evaluate it, but the definition itself doesn't
-- print the IO.

-- We need to use the join function to strip off a layer of IO and join them up:
works :: IO ()
works = join $ putStrLn <$> getLine

-- List Monad
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]

{-- The Maybe Monad
instance Monad Maybe where
    return x = Just x
    (Just x) >>= k = k x
    Nothing >>= _ = Nothing
--}

data Cow = Cow {
    name :: String,
    age :: Int,
    weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)

-- The weightCheck function is not possible with just applicative, because it
-- takes an already constructed cow and returns a Maybe Cow, thus injecting more
-- structure into the Maybe that gets flattened out with join.

-- Either Monad
type Founded = Int
type Coders = Int
data SoftwareShop =
    Shop {
        founded :: Founded,
        programmers :: Coders
    } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers


data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (Second f) <*> (Second b) = Second (f b)
    (First a) <*> _ = First a
    _ <*> (First a) = First a

instance Monad (Sum a) where
    return = pure
    (First a) >>= _ = First a
    (Second b) >>= k = k b

--  ===========================================================================
--                             Monad Laws
--  ===========================================================================
{-- 1. Left Identity
m >>= return = m

-- 2. Right Identity
return x >>= f = f x

-- 3. Associativity
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
--}

-- The laws can be checked with quickBatch:
q1 = quickBatch $ monad (undefined :: [(Int, Int, Int)])


-- Kleisli Composition
-- If I have two monads f and g and I'd like to compose them, I need something like this:
mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp f g a = g a >>= f

-- This can be written with the >=> operator

--  ===========================================================================
--                          Chapter Exercises
--  ===========================================================================
--  The simplest Monad imaginable
data Nope a = NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

instance Monad Nope where
    return = pure
    _ >>= _ = NopeDotJpg

q2 = quickBatch $ monad (undefined :: Nope (Int, Int, Int))

-- Identity Monad
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
    return = pure
    (Identity a) >>= k = k a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

q3 = quickBatch $ monad (undefined :: Identity (Int, Int, Int))

checks = q1 >> q2 >> q3

-- Find that function!
j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = (<**>)


iToS :: Int -> Maybe String
iToS x = if x > 5 then Just ("yup: " ++ show x) else Nothing

-- TODO: Come back to this one and the following question!
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = undefined
{--
meh xs f = let res = map f xs
               joined = map join res
           in undefined
--}

nothin = meh [1..10] iToS
justListStr = meh [6..12] iToS
