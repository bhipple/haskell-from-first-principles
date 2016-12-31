{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module Ch30_when_things_go_wrong
where

import Control.Exception
import Data.Typeable

-- Goals:
-- * Examine the Exception typeclass and methods
-- * Explore Existential Quantification
-- * Discuss exception handling approaches

{-- The Exception typeclass
class (Typeable e, Show e) => Exception e where
    toException :: e -> SomeException
    fromException :: SomeException -> Maybe e
    displayException :: e -> String
--}
-- Note that SomeException is itself an instance of the Exception typeclass

-- Existential Quantification
-- SomeException is a parent type for all other exception types that allows us
-- to handle exceptions with having to pattern match on all of them
data SomeException' =
    forall e . Exception e => SomeException' e
-- Roughly, "forall e such that e is an Exception, e is a SomeException'"
-- This allows for a polymorphic argument in the data constructor, instead
-- of a polymorphic argument in the type constructor!

-- There is also generalized algebraic datatype (GADT) syntax to do this:
data SomeException'' where
        SomeException'' :: Exception e => e -> SomeException''

-- Here we'll make a simple exception to show the functionality, without
-- all the actual Exception typeclass magic.
data MyException = forall e . (Show e, Typeable e) => MyException e

instance Show MyException where
    showsPrec p (MyException e) = showsPrec p e

-- Note that MyException here can hold DivideByZero and StackOverflow
-- without resorting to a sum type or exposing polymorphic type arguments
-- in the type constructor, even though those are two distinct types.
multiError :: Int -> Either MyException Int
multiError n =
    case n of
      0 -> Left (MyException DivideByZero)
      1 -> Left (MyException StackOverflow)
      _ -> Right n

data SomeError = Arith ArithException
               | Async AsyncException
               | SomethingElse
               deriving (Show)

discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
    case cast e of
      (Just arith) -> Arith arith
      Nothing ->
          case cast e of
            (Just async) -> Async async
            Nothing -> SomethingElse

-- Note that this doesn't need a sum type for the various exceptions that can be thrown!
runDisc :: Int -> SomeError
runDisc n = either discriminateError
                   (const SomethingElse) (multiError n)

-----------
-- Typeable
-- Allows types to be known at runtime for dynamic typechecking.
-- Defines one method:
cast' :: (Typeable a, Typeable b) => a -> Maybe b
cast' = undefined
-- When an exception is caught, the cast is used to see if the Exception
-- can be cast into an exception that the catch statement catches. If it can't
-- be cast, the exception propagates onward up the stack.

-- `catch` can be used to run an action and install an exception handler to catch
-- some or all exceptions
-- `try` can be used to run an action and lift its result into IO (Either e a)
-- `catches` can be used if we want multiple handlers for multiple exception types

-- Note that exceptions are not really for catching bottoms; write total programs!
-- Due to laziness, the bottom may be forced after the try/catch is already over:
justFine :: IO (Either SomeException ())
justFine = try undefined

explodes :: IO (Either SomeException())
explodes = try $ return undefined

-- Asynchronous Exceptions
-- These are exceptions raised from a different thread than the one that'll receive the error.
-- It allows threads to communicate with each other in rich ways, if used carefully.
-- If a child thread wants to avoid death for some time (to avoid leaking resources or
-- leaving things in an inconsistent state), it can use the mask_ function -- though
-- this can result in the parent's exception being caught in the parent thread instead
-- of the child thread, so be careful!
