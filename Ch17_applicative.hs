module Ch17 where
import Control.Applicative (liftA2, liftA3)
import Data.List (elemIndex)
import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Applicative is a monoidal functor, in a sense.
-- It allows for function application lifted over structure, but the function we're
-- applying is itself embedded in some structure.
--
{-- Definition
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
--}

ex1 = [(*2), (*3)] <*> [4, 5]
ex2 = Just (*2) <*> Just 3
ex3 = ("Woo", (+1)) <*> ("hoo!", 0) :: (String, Int)

{-- The last example works beause we have:
instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u <> v, f x)
-- Note that as with Functor, Applicative takes an argument of kind * -> *
--}

-- For convenience, if we have a function we want to lift into a functor
-- and then apply with another functor, we can use liftA2
equiv = ((,) <$> [1, 2] <*> [3, 4]) == liftA2 (,) [1, 2] [3, 4]

added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1..3] [4..6])

y' :: Maybe Integer
y' = lookup 3 (zip [1..3] [4..6])

z :: Maybe Integer
z = lookup 2 (zip [1..3] [4..6])

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y' <*> z

x :: Maybe Int
x = elemIndex 3 [1..5]

y :: Maybe Int
y = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

-- All of the below are equivalent
maxed :: Maybe Int
maxed = max' <$> x <*>  y
maxed' = liftA2 max' x y
maxed'' = pure max' <*> x <*> y

xs = [1..3]
ys = [4..6]

x4 = lookup 3 $ zip xs ys
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x4 <*> y4)
summed' = pure sum <*> ((,) <$> x4 <*> y4)
summed'' = sum <$> liftA2 (,) x4 y4

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

ai1 = const <$> [1..3] <*> [6..9]
ai2 = const <$> Identity [1..3] <*> Identity [6..9]

-- Constant Applicative Type
newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap f (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant x) <*> (Constant y) = Constant (x <> y)

c1 = Constant (Sum 1) <*> Constant (Sum 2)
c2 = Constant undefined <*> Constant (Sum 2)

-- Maybe Applicative
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if length s > maxLen then Nothing
                         else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

data Person = Person Name Address
    deriving (Eq, Show)

-- A verbose "smart constructor" for Person
mkPerson :: String -> String -> Maybe Person
mkPerson n a =
    case mkName n of
        Nothing -> Nothing
        Just n' ->
            case mkAddress a of
                Nothing -> Nothing
                Just a' -> Just $ Person n' a'

-- This does the same as above, leveraging the power of applicative!
mkPerson' n a = Person <$> mkName n <*> mkAddress a

e1 = const <$> Just "Hello" <*> pure " World"
e2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1..3]

-- ============================================================================
--                             Applicative Laws
-- ============================================================================
{-- 1. Identity
pure id <*> v = v

-- 2. Composition
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

-- 3. Homomorphism
pure f <*> pure x = pure (f x)
-- A homomorphism is a structure-preserving map between two categories
-- Generally, this means the <*> application doesn't change the structure

-- 4. Interchange
u <*> pure y = pure ($ y) <*> u
--}

-- ============================================================================
--               Checking with the QuickCheck Checkers library
-- ============================================================================
data Bull = Fools | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools),
                           (1, return Twoo)]

-- Busted monoid definition
instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

instance EqProp Bull where (=-=) = eq

-- How freakin' awesome is this?
-- Test whether Bull as written is a valid Monoid
q1 = quickBatch $ monoid Twoo

-- Test whether [(String, String, Int)] tuples are valid Applicatives
q2 = quickBatch $ applicative ([("b", "w", 1)] :: [(String, String, Int)])

-- Note that the values passed are not evaluated; they're just used to
-- evaluate the type to use.
trigger = undefined :: [(String, String, Int)]
q3 = quickBatch $ applicative trigger


-- List Applicative Exercise
data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

instance Monoid (List a) where
    mempty = Nil
    Nil `mappend` y = y
    Cons x xs `mappend` ys = Cons x $ xs `mappend` ys

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold mappend Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

applyToAll :: List (a -> b) -> List a -> List (List b)
applyToAll Nil _ = Nil
applyToAll _ Nil = Nil
applyToAll (Cons f fs) ys = Cons (fmap f ys) (applyToAll fs ys)

-- Here, we want to take every function on the left, apply it to
-- every value on the right, and squish them together.
instance Applicative List where
    pure = const Nil
    -- TODO: This works, but there should be a prettier way of doing it without
    -- relying on a helper function that pattern matches on the Cons constructor.
    -- (Using just flatMap and fmap)
    fs <*> ys = concat' $ applyToAll fs ys

-- Test data
funcs = Cons (+1) (Cons (*2) Nil)
vals = Cons 1 (Cons 2 Nil)

-- Should give: Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
demo = funcs <*> vals

-- ============================================================================
--                            ZipList Applicative
-- ============================================================================
newtype ZipList' a = ZipList' (List a)
    deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x xs) = Cons x $ take' (n-1) xs

-- Some added complexity on testing for "equality" in infinite sets ...
instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in take' 3000 l
              ys' = let (ZipList' l) = ys
                    in take' 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

cycleList :: List a -> List a
cycleList Nil = Nil
cycleList xs = xs' where xs' = xs <> xs'

zipLists :: List (a -> b) -> List a -> List b
zipLists Nil _ = Nil
zipLists _ Nil = Nil
zipLists (Cons f fs) (Cons y ys) = Cons (f y) (zipLists fs ys)

instance Applicative ZipList' where
    pure x = ZipList' (Cons x Nil)
    ZipList' fs <*> ZipList' ys = ZipList' $ zipLists (cycleList fs) ys

zvals = ZipList' $ Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
zfuncs = ZipList' $ Cons (+1) (Cons (+2) (Cons (+3) Nil))
zdemo = zfuncs <*> zvals
zid = pure id <*> zvals

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [(1, return Nil),
                           (4, Cons <$> arbitrary <*> arbitrary)]

-- Take a look at some arbitrary sequences with
-- sample (arbitrary :: Gen (List Int))
-- Pretty nifty!

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = ZipList' <$> arbitrary

-- TODO: ZipList' is failing composition and interchange laws
q4 = quickBatch $ applicative (undefined :: ZipList' (Int, Int, List Int))

-- Either Applicative
data Either' a b = Left' a | Right' b
    deriving (Eq, Show)

instance Functor (Either' a) where
    fmap _ (Left' x) = Left' x
    fmap f (Right' a) = Right' (f a)

instance Applicative (Either' b) where
    pure = Right'
    Left' x <*> _ = Left' x
    _ <*> Left' x = Left' x
    Right' x <*> Right' y = Right' (x y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either' a b)  where
    arbitrary = frequency [(1, Left' <$> arbitrary),
                           (3, Right' <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Either' a b)
    where (=-=) = eq

q5 = quickBatch $ applicative (undefined :: (Either' String) (Int, Int, Int))

-- Validation Applicative, which is the same as Either except it takes monoidal
-- errors and combines them.
data Validation e a =
    Error' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Error' e) = Error' e
    fmap f (Success' a) = Success' (f a)

instance (Monoid e) => Applicative (Validation e) where
    pure = Success'
    Success' f <*> Success' a = Success' (f a)
    Error' e1 <*> Error' e2 = Error' (e1 <> e2)
    _ <*> Error' e = Error' e
    Error' e <*> _ = Error' e

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = frequency [(1, Error' <$> arbitrary)
                          ,(3, Success' <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Validation a b)
    where (=-=) = eq

q6 = quickBatch $ applicative (undefined :: (Validation String) (String, Int, Int))

v1 = Error' "Error A; " :: Validation String (Int -> Int)
v2 = Error' "Error B; " :: Validation String Int
v3 = Success' (+5) :: Validation String (Int -> Int)
v4 = Success' 3 :: Validation String Int

f1 = v1 <*> v2
s1 = v3 <*> v4
f2 = v3 <*> v2

qchecks = q1 >> q2 >> q3 >> q4 >> q5 >> q6

--  ===========================================================================
--                          Chapter Exercises
--  ===========================================================================
combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

stops, vowels :: String
stops = "xyz"
vowels = "aeiou"

stopsVowelsStops = combos stops vowels stops

-- TODO: Come back to the writing of other Applicative instances as a refresher
-- later.
