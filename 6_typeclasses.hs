module Ch6 where

-- Important typeclasses:
-- Num
-- Fractional
-- Eq
-- Ord

-- Enum: types that are enumerable with known pred and succ
class Enum a where
    succ :: a -> a
    pred :: a -> a
    toEnum :: Int -> a
    fromEnum :: a -> Int
    enumFrom :: a -> [a]
    enumFromThen :: a -> a -> [a]
    enumFromTo :: a -> a -> [a]
    enumFromThenTo :: a -> a -> a -> [a]

-- This function is really cool!
ex1 :: [Int]
ex1 = Prelude.enumFromThenTo 1 10 100

class Show' a where
    showsPrec' :: Int -> a -> ShowS
    show' :: a -> String
    showList' :: [a] -> ShowS

-- A trivial typeclass
data Trivial = Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

-- Dates!
data DayOfWeek =
    M | Tu | W | Th | F | Sa | Su

data Date =
    Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) M M = True
    (==) Tu Tu = True
    (==) W W = True
    (==) Th Th = True
    (==) F F = True
    (==) Sa Sa = True
    (==) Su Su = True
    (==) _ _ = False

instance Eq Date where
    (==) (Date weekday monthNum)
         (Date weekday' monthNum') =
         weekday == weekday' && monthNum == monthNum'


-- With the -Wall ghc flag, we can get ghc to check all
-- functions to make sure they're total!  It will complain about 
-- any partial functions.
--
-- In particular, watch out for functions that are taking a casual
-- Int type when really they should be taking a sum data type like
-- DayOfWeek.  Making the first type of function total with int
-- requires writing a catch-all case that probably handles more invalid
-- input than we'd like.

-- Exercises
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
    (==) (TisAn i) (TisAn i') = i == i'

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt =
    TisAnInt Int
  | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt i) (TisAnInt i') = i == i'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) _ _ = False

-- Note that for this one, we have to constraint our type a to
-- types which are themselves instances of Eq
data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data EitherOr a b =
    Hello a
  | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'
    (==) _ _ = False


-- Exercises
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson = putStrLn . show

data Mood = Blah
          | Woot deriving (Eq, Show)

settleDown x = if x == Woot
                 then Blah
                 else x

data Phrase = Phrase String String String deriving (Eq, Show)
p1 = Phrase "Foo" "Bar"

i :: Num a => a
i = 1

-- Type-Kwon-Do
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f x y = (f y) + fromIntegral x
