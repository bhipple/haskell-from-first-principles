module Ch9 where

import Data.Char

-- The :sprint command in GHCi can be used to see what's been evaluated and
-- what's a thunk waiting to be evaluated! Note that values that are not fully
-- concrete types like [1..10] (Num) can't be evaluated, since they're still
-- waiting for a better constraint.

-- Spines and Values
-- In Haskell data structures, spines are the structure that glue the data
-- structure together, holding a bunch of values in order. Spines and values are
-- evaluated independently. For example:
undefList = [undefined, undefined, undefined]
three = length undefList
-- error = head undefList

-- Weak Head Normal Form (WHNF) is the default reduction for values in Haskell.
-- Normal form means the expression is fully evaluated
-- WHNF means the expression is only evaluated as far as is necessary to reach a data constructor or lambda awaiting an argument.

-- Some functions, like length, are strict in evaluation of the spine but lazy otherwise.
-- Others, like sum, are strict in both.

-- For performance sensitive code, remember the mantra "lazy in the spine, strict in the leaves".

mult3 = filter (\x -> rem x 3 == 0) [1..30]

article :: String -> Bool
article s =
    case s of
        "the" -> True
        "a" -> True
        "an" -> True
        _ -> False

noArticles :: String -> [String]
noArticles = filter (not . article) . words

ex1 = noArticles "the brown dog was a goof"

uppers :: String -> String
uppers = filter isUpper

ce1 = uppers "AbCdEf"
