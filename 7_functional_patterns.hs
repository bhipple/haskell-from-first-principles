module Ch7 where

-- Lambda functions, pattern matching
newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
    = putStrLn $ name ++ " " ++ show acctNum


-- In GHCI, the :browse function lets you browse symbols imported from a module.

-- Exercises
-- Look how easy this is with pattern matching!
f :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
f (a,b,c) (d,e,f) = ((a,d), (c,f))

-- Rewriting if then else with case syntax
funcC x y = if x > y then x else y
funcC' x y =
    case x > y of
        True -> x
        False -> y

ifEvenAdd2 n = if even n then n + 2 else n
ifEvenAdd2' n =
    case even n of
        True -> n + 2
        False -> n

nums x =
    case compare x 0 of
        LT -> -1
        EQ -> 0
        GT -> 1

-- Chapter Exercises
g :: (a -> b) -> (a, c) -> (b,c)
g f (a, c) = (f a, c)

-- There and back again
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

ex = do
    print (roundTrip 4)
    print (roundTrip' 4)
    print (roundTrip'' 4 :: Int)
