{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Ch24 where
-- A Parser Combinator is a higher-order function that takes parsers as input
-- and returns a new parser as output.
-- Combinators are expressions with no free variables
import Text.Trifecta
import Text.Parser.Combinators

import Control.Applicative
import Data.Ratio ((%))
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Data.Attoparsec.Text (parseOnly)
import Data.String (IsString)
import Data.Aeson hiding (Result, Success)

import qualified Data.ByteString.Lazy as LBS

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

-- Parses Either Rational Double. We use try to avoid consuming
-- input in the case when double fails; if the first parser consumes
-- any input before failing, the altnerate branch of <|> will not be followed
parseNum :: Parser (Either Double Rational)
parseNum = Left <$> try double
       <|> Right <$> parseFraction

--  ===========================================================================
--                           INI File Parser
--  ===========================================================================
newtype Header = Header String
    deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

type Name = String
type Val = String
type Assignments = Map Name Val

parseAssignment :: Parser (Name, Val)
parseAssignment = do
    name <- some letter
    char '='
    val <- some (noneOf "\n")
    skipEOL
    return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments = skipMany (do _ <- char ';' <|> char '#'
                            skipMany (noneOf "\n")
                            skipEOL)

data Section = Section Header Assignments
    deriving (Eq, Show)

newtype Config = Config (Map Header Assignments)
    deriving (Eq, Show)

skipWhitespace :: Parser()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
    skipWhitespace
    skipComments
    h <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)

-- Combine sections into a map that keys section data by section name
rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections = foldr rollup M.empty sections
    return (Config mapOfSections)

-- Example INI file data
headerEx :: ByteString
headerEx = "[blah]"

assignmentEx :: ByteString
assignmentEx = "woot=1"

commentEx :: ByteString
commentEx = "; it has been 3 days since this comment was updated"

commentEx' :: ByteString
commentEx' = "; foo\n; bar\n  \n; hah"

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

t1 = parseByteString parseIni mempty sectionEx''

-- Testing / Main
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
    describe "Assignment Parsing" $
        it "can parse a simple assignment" $ do
            let m = parseByteString parseAssignment mempty assignmentEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just ("woot", "1")
    describe "Header Parsing" $
        it "can parse a simple header" $ do
            let m = parseByteString parseHeader mempty headerEx
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "blah")
    describe "Comment parsing" $
        it "Can skip a comment before a header" $ do
            let p = skipComments >> parseHeader
                i = "; woot\n[blah]"
                m = parseByteString p mempty i
                r' = maybeSuccess m
            print m
            r' `shouldBe` Just (Header "blah")
    describe "INI parsing" $
        it "Can parse multiple sections" $ do
            let m = parseByteString parseIni mempty sectionEx''
                r' = maybeSuccess m
                sectionValues = M.fromList [("alias", "claw")
                                          , ("host", "wikipedia.org")]
                whatisitValues = M.fromList [("red", "intoothandclaw")]
                expected' = Just (Config (M.fromList [(Header "section", sectionValues)
                                                    , (Header "whatisit", whatisitValues)]))
            print m
            r' `shouldBe` expected'

--  ===========================================================================
--                         Polymorphic Parsers
--  ===========================================================================
-- Using generic types, we can get parsers compatible with
-- attoparsec, trifecta, parsec, etc.
badFraction' :: IsString s => s
badFraction' = "1/0"

parseFraction' :: (Monad m, TokenParsing m) => m Rational
parseFraction' = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

demo3 = do
    -- Attoparsec
    print $ parseOnly parseFraction' badFraction'

    -- Trifecta
    print $ parseString parseFraction' mempty badFraction'

-- That said, be aware that there are differences in failure and backtracking
-- handling between the major parsers.  Parsec's <|> does not automatically backtrack,
-- while attoparsec and trifecta do. Trifecta and Attoparsec have consistent behavior,
-- though.

-- Marshalling and unmarshalling JSON data
sectionJson :: LBS.ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
"whatisit": {"red": "intoothandclaw"}
}
|]

data TestData = TestData {
    section :: Host
  , what :: Color
} deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

type Annotation = String

data Color =
      Red Annotation
    | Blue Annotation
    | Yellow Annotation
    deriving (Eq, Show)

instance FromJSON TestData where
    parseJSON (Object v) =
        TestData <$> v .: "section"
                 <*> v .: "whatisit"
    parseJSON _ = fail "Expected an object for TestData"

instance FromJSON Host where
    parseJSON (Object v) = Host <$> v .: "host"
    parseJSON _ = fail "Expected an object for Host"

instance FromJSON Color where
    parseJSON (Object v) = (Red <$> v .: "red")
                    <|> (Blue <$> v .: "blue")
                    <|> (Yellow <$> v .: "yellow")
    parseJSON _ = fail "Expected an object for Color"

demo4 = do
    let d = decode sectionJson :: Maybe TestData
    print d

--  ===========================================================================
--                          Chapter Exercises
--  ===========================================================================
-- Parser for Semantic Versions
data NumberOrString = NOSS String | NOSI Integer
    deriving (Eq, Ord, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
    deriving (Eq, Show, Ord)

parseSemVer :: Parser SemVer
parseSemVer = undefined

ts1 = parseString parseSemVer mempty "2.1.1"
ts2 = parseString parseSemVer mempty "1.0.0-x.7.z.92"
ts3True = SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []

-- TODO: Come back to this later
