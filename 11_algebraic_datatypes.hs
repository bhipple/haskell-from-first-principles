{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Ch11 where

-- HuskyType is a parametrically polymorphic type constructor with a single
-- type variable element. Note that HuskyData is a type constant, known as a "phantom" type.
data HuskyType a = HuskyData

data Price = Price Integer deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsRUs
             | TakeYourChancesUnited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
yourCar = Car Mazda (Price 20000)
doge = Plane PapuAir

-- Exhaustive with manual iteration
isCar :: Vehicle -> Bool
isCar (Car m p) = True
isCar (Plane a s) = False

-- Exhaustive with the otherwise catch-all
isPlane :: Vehicle -> Bool
isPlane (Plane a s) = True
isPlane v = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- Blows up if plane is given
getManu :: Vehicle -> Manufacturer
getManu (Car m p) = m


-- Data Constructor Arities
-- Nullary => Takes no arguments
-- Unary => 1 arg
-- Binary => 2 args, and so on
-- Data structures that take more than 1 argument are called products

-- We define the cardinality of a data type as the number of
-- values that it can possibly take. Calculating the cardinality
-- of data types tells us how many possible values we have to reason
-- about in a function signature.

-- Newtype
-- Consider these two functions newtypes and the following function:
newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42
tooManyGoats' :: Goats -> Bool
tooManyGoats' (Goats n) = n > 42

-- Newtype can only take a unary constructor, which means the type
-- does not need to persist through to runtime (no overhead).
-- However, it gets us nice static compile-time type checking,
-- unlike type aliases!

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

-- With the GeneralizedNewtypeDeriving language pragma, we don't
-- need to define this if we want it to just be n > 42
instance TooMany Goats where
    tooMany (Goats n) = n > 43

-- Requires LANGUAGE FlexibleInstances
instance TooMany (Int, String) where
    tooMany (n, s) = n > 42

instance TooMany (Int, Int) where
    tooMany (x, y) = x + y > 42

-- Product types are like structs in C. Record types are product types
-- with some extra syntactic sugar to provide convenient accessors
-- into the fields.

-- A product type:
data Person = MkPerson String Int deriving (Eq, Show)

-- With record syntax:
data Person' =
    Person' { name :: String
            , age :: Int }
            deriving (Eq, Show)

-- We then get these helpers for free:
-- name :: Person -> String
-- age :: Person -> Int
