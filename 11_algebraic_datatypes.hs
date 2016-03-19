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

-- ============================================================================
--                                Normal Form
-- ============================================================================
data Fiction = Fiction deriving (Eq, Show)
data Nonfiction = Nonfiction deriving (Eq, Show)

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving (Eq, Show)

type AuthorName = String

-- Algebraic data types also support the distributive propery, with product types distributing
-- over sum types.  This is not normal form:
data Author = Author (AuthorName, BookType) deriving (Eq, Show)

-- Types are in "normal form" when they are written as a sum of product types
{-- Not sure why this doesn't compile ...
data Author' =
    Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)
-}

-- ============================================================================
--                      Construction and Deconstruction
-- ============================================================================
data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

newtype NumCow =
    NumCow Int
    deriving (Eq, Show)

newtype NumPig =
    NumPing Int
    deriving (Eq, Show)

data Farmhouse =
    Farmhouse NumCow NumPig
    deriving (Eq, Show)

-- This is equivalent to Farmhouse
type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
    NumSheep Int
    deriving (Eq, Show)

-- These two are also equivalent thanks to distributivity of product types
-- across sum types, with nested products
data BigFarmhouse =
    BigFarmhouse NumCow NumPig NumSheep
    deriving (Eq, Show)

type BigFarmhouse' =
    Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo =
    CowInfo Name Age
    deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

-- Equivalent
data Animal' =
    Sum CowInfo (Sum PigInfo SheepInfo)

-- By using nullary constructed types instead of string aliases,
-- the type system can do some work checking usages for us
-- e.g., if someone tries 'Second Twitter :: Sum Twitter AskFm' it'll give a type error
data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

-- In the sum type disjunction, we only have one or the other but not both.
-- The type system will fail to compile if we try to pull the wrong one out.
socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data RecordProduct a b =
    RecordProduct { pfirst :: a
                  , psecond :: b }
                  deriving (Eq, Show)

-- ============================================================================
--                          Programmer OS Exercise
-- ============================================================================
data OperatingSystem =
    GnuPlusLinux
  | OpenBSD
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgrammingLanguage }
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSD, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

-- Since type constructors are functions, we can map over them with a list comprehension :)
allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]


-- ============================================================================
--                            Cardinality, again
-- ============================================================================
data Quantum = No | Yes | Both deriving (Eq, Show)

-- Sum types are additive, so this has 6 types:
data HasSixValues = Either Quantum Quantum

-- Products types are multiplicative, so:
data HasNineProductValues = NineValues (Quantum, Quantum)

-- Functions are exponential, such that a -> b -> c == (c ^ b) ^ a
has27Implementations :: Quantum -> Quantum
has27Implementations = undefined

has8Implementations :: Quantum -> Bool
has8Implementations = undefined

-- Constructor notes
-- Any constructor starting with a non-alphanumeric character is infix by default

-- ============================================================================
--                               Binary Trees
-- ============================================================================
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)


