{-# LANGUAGE InstanceSigs #-}
module Ch23 where
import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
-- State Monad
-- Like reader, this is a newtype wrapper:
{--
newtype State s a =
    State { runState :: s -> (a, s) }
--}
-- This facilitates patterns where we have some state that will be used
-- to generate a value, returning the value and the updated state.
-- E.g., generation of random numbers, which takes a gen and returns an int
-- and a new gen.
-- We use this monad transformer helper function:
-- state :: Monad m => (s -> (a, s)) -> StateT s m a

-- Writing it ourselves
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in (f a, s')

oneZero = runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi $ \s ->
            let (ab, s1) = f s
                (a, s2) = g s1
            in (ab a, s2)

instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi $ \s ->
            let (a, s1) = f s
            in runMoi (g a) s

-- FizzBuzz with the State Monad!
fizzbuzz :: Integer -> String
fizzbuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Fizz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList lst = execState (mapM_ addResult lst) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzbuzz n
    put (result : xs)

main :: IO ()
main = mapM_ putStrLn $ reverse $ fizzbuzzList [1..10]

--  ===========================================================================
--                          Chapter Exercises
--  ===========================================================================
-- State where state is also value
get' :: State s s
get' = state $ \s -> (s, s)

-- State where resulting state is argument and value is unit
put' :: s -> State s ()
put' s = state $ \s -> ((), s)

-- Run state with s and get the state that results
exec :: State s a -> s -> s
exec sa s = snd $ runState sa s

-- Run state with s and get the value
eval :: State s a -> s -> a
eval sa s = fst $ runState sa s

-- Applies a function to create new state
modify'' :: (s -> s) -> State s ()
modify'' f = state $ \s -> ((), f s)

empty1 = runState (modify'' (+1)) 0
empty2 = runState (modify'' (+1) >> modify'' (+1)) 0
