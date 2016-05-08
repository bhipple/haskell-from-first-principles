module Ch16 where
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
