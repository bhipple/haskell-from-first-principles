{-# LANGUAGE OverloadedStrings #-}
module Ch24 where
-- A Parser Combinator is a higher-order function that takes parsers as input
-- and returns a new parser as output.
-- Combinators are expressions with no free variables
import Text.Trifecta
import Text.Parser.Combinators

import Control.Applicative
import Data.Ratio ((%))

-- Fail with an error message
stop :: Parser a
stop = unexpected "stop"

-- Parse the character '1'
one :: Parser Char
one = char '1'

-- Read a '1', then throw it away and stop
one' = one >> stop

{-- Parsers behave much like the State monad:
type Parser a = String -> Maybe (a, String)
--}
-- They await a string value, produce a result which may or may not succeed,
-- and return a tuple of the value you wanted and whatever's leftover that was not
-- consumed from the string.

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

demo = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'

-- Parser that fails if it doesn't exhaust input stream
oneEnd = do
    a <- one
    eof
    return a

oneTwoEnd = do
    a <- oneTwo
    eof
    return a

oneTwoThree :: Parser Char
oneTwoThree = char '1' >> char '2' >> char '3'


-- Parsing Fractions
badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

demo2 = do
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldAlsoWork
    print $ parseString parseFraction mempty alsoBad
    print $ parseString parseFraction mempty badFraction

ex1 = parseString (integer >> eof) mempty "123"

getInt = do
    i <- integer
    eof
    return i

ex1' = parseString getInt mempty "123"
