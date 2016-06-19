{-# LANGUAGE RankNTypes #-}
module Ch16 where
import Test.QuickCheck
import Test.QuickCheck.Function

-- Functors
{-- Typeclass definition:
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- Laws:
-- Identiy Law:
fmap id == id

-- Composition Law:
fmap (f . g) == fmap f . fmap g
-}

-- The identity law enforces the constraint that the functor does not change
-- the outer structure of f, just the internal values.  The second law also
-- deals with preservation of structure, this time by allowing us to compose
-- functions.

-- Note that in function definitions, all paremeter variables have to have
-- the concrete kind type *.
class Sumthin a where
    s :: a -> a
-- Here, the a has to have kind *

class Else f where
  e :: b -> f (g a b c)
-- a, b, c :: *
-- g :: * -> * -> * -> *
-- f :: * -> *

class Biffy e where
  slayer :: e a b -> (a -> c) -> (b -> d) -> e c d
-- a, b, c, d :: *
-- e :: * -> * -> *

replaceWithP = const 'p'

-- If we have nested functors (list of maybe of string), we can stack up fmaps
-- This is pretty cool!  See examples below
lms = [Just "Ave", Nothing, Just "woohoo"] :: [Maybe String]

p = replaceWithP lms :: Char
ppp = fmap replaceWithP lms :: String
mp3 = (fmap . fmap) replaceWithP lms :: [Maybe Char]
allP = (fmap . fmap . fmap) replaceWithP lms :: [Maybe String]

lmls :: [Maybe [String]]
lmls = [Just ["Ha", "Ha"], Nothing, Just []]

fe0 = replaceWithP lmls
fe1 = fmap replaceWithP lmls
fe2 = (fmap . fmap) replaceWithP lmls
fe3 = (fmap . fmap . fmap) replaceWithP lmls
fe4 = (fmap . fmap . fmap . fmap) replaceWithP lmls

-- This is the general type for a thrice lifted fmap
thriceLifted :: (Functor f2, Functor f1, Functor f) =>
                f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- However, we can make it more specific in the case when applying to lms, to
-- see specific types:
thriceLifted' :: [Maybe String] -> [Maybe String]
thriceLifted' = thriceLifted
-- f ~ []
-- f1 ~ Maybe
-- f2 ~ [] (the [Char] inside the Maybe)
-- Now thriceLifted lms == thriceLifted' lms

-- Lifting Exercises
l1 = (+1) <$> read "[1]" :: [Int]
l2 = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"] :: Maybe [String])
l3 = fmap (*2) (\x -> x - 2) 1
l4 = fmap ((return '1' ++) . show) (\x -> [x, 1..3]) 0
l5 :: IO Integer
l5 = let ioi = readIO "1" :: IO Integer
         changed = fmap (read . ("123" ++) . show) ioi
     in fmap (*3) changed

-- When we fmap over tuples like (a,b), Either, Two, etc., the fmap
-- only impacts the second type.  Why?  Because the kind of Either is
-- * -> * -> *, but Functor needs a * -> *, so the instance declaration
-- for Either as a functor is
-- instance Functor (Either a) where { fmap = ... }
-- This binds a generic type a and makes the functor operate only on the last
-- type!  So the decision isn't arbitrary about why it impacts the second type
-- instead of the first.
data Two a b = Two a b deriving (Eq, Show)
data Or a b = First a | Second b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

-- Using QuickCheck
funcIdent :: (Functor f, Eq (f a)) => f a -> Bool
funcIdent f = fmap id f == f

funcCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
funcCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- This one can be used with generic QuickCheck function generation
funcCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
funcCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

ex1 = quickCheck $ \x -> funcIdent (x :: [Int])
ex2 = quickCheck $ \x -> funcCompose (+1) (*2) (x :: [Int])
ex3 = quickCheck (funcCompose' :: [Int] -> Fun Int Int -> Fun Int Int -> Bool)

-- Implementing Functor instances and using QuickCheck validation
newtype Identity a = Identity a
    deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

i1 = quickCheck $ \x -> funcIdent (x :: Identity Int)
c1 = quickCheck (funcCompose' :: Identity Int -> Fun Int Int -> Fun Int Int -> Bool)

data Pair a = Pair a a
    deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

i2 = quickCheck $ \x -> funcIdent (x :: Pair Int)
c2 = quickCheck (funcCompose' :: Pair Int -> Fun Int Int -> Fun Int Int -> Bool)

-- Using Two a b declaration from above
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

i3 = quickCheck $ \x -> funcIdent (x :: Two Int String)
c3 = quickCheck
        (funcCompose' :: Two Int String -> Fun String String -> Fun String String -> Bool)
--                                              ^ Note the type!

data Three a b c = Three a b c
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

i4 = quickCheck $ \x -> funcIdent (x :: Three Double String Int)
c4 = quickCheck (funcCompose' :: Three Double String Int -> Fun Int Int ->
                                                            Fun Int Int ->
                                                            Bool)

data Three' a b = Three' a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

i5 = quickCheck $ \x -> funcIdent (x :: Three' String Int)
c5 = quickCheck (funcCompose' :: Three' String Int -> Fun Int Int -> Fun Int Int -> Bool)

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
    arbitrary = return Trivial

-- Note that while we can implement Arbitrary for Trivial, we can't implement
-- functor!  Functor takes an argument that has kind * -> *, while Trivial has
-- kind *
checks = ex1 >> ex2 >> ex3 >> i1 >> c1 >> i2 >> c2 >> i3 >> c3 >> i4 >> i5 >> c5

-- Because fmap doesn't touch the left side in Maybe and Either,
-- we can safely keep fmapping over chains of results without worrying about
-- it interacting in the event of a failure!
data Possibly a = LolNope | Yeppers a
    deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

-- A funky wrapping functor. The second type a is an argument to the
-- type constructor f
data Wrap f a = Wrap (f a)
    deriving (Eq, Show)

-- So if f is itself a functor, then we can get a functor instance for
-- (Wrap f a) like so:
instance Functor f => Functor (Wrap f) where
    fmap g (Wrap fa) = Wrap (fmap g fa)

wf1 = fmap (+1) (Wrap (Just 1))
wf2 = fmap (+1) (Wrap [1, 2, 3])


-- Natural Transformations
-- While functors transform data while maintaining structure, natural transformations
-- do the opposite: transform the structure while leaving the data.
-- This requires the RankNTypes directive, to get around the fact that f and g are
-- higher kinded types that we want to use as function arguments.
type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- Note that because a is any type, we can't possibly do anything with it -- like the id function.

-- The type system and laws make the Functor instance implementation for a given
-- datatype unique!

---- ============================================================================
----                             Chapter Exercises
---- ============================================================================
data Sum b a = First' a | Second' b

instance Functor (Sum b) where
    fmap f (First' a) = First' (f a)
    fmap _ (Second' b) = Second' b

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More b) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

l224 = fmap (+1) (L 1 2 3)
r133 = fmap (+1) (R 1 2 3)
