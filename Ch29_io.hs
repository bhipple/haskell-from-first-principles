{-# LANGUAGE OverloadedStrings #-}
module Ch29_io
where

import System.FilePath
import Control.Concurrent
import Data.ByteString (ByteString)
import Data.List (isSuffixOf)
import Debug.Trace
import System.Directory
import System.Random
import Text.Trifecta
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Map (Map)

-- For the chapter exercises
import Ch24_parser_combinators

-- Under normal circumstances, GHC is free to do reordering of operations,
-- delay evaluation, share named values, duplicate code via inlining, and
-- other performance optimizations. Largely, what IO does is turn most of that off.

myData :: IO (MVar Int)
myData = newEmptyMVar

-- Since we have two different MVars here, the second take dies.
-- This is because the IO type on myData prevents sharing!
mvarDeadlockDemo :: IO ()
mvarDeadlockDemo = do
    mv <- myData
    putMVar mv 0
    mv' <- myData
    zero <- takeMVar mv'
    print zero

-- IO *only* prevents sharing for the terminal value it reduces to. Values not
-- dependent on IO for their evaluation can still be shared, even within a larger
-- IO action such as main.
blah :: IO String
blah = return "blah"

blah' :: IO String
blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

ioShareDemo :: IO ()
ioShareDemo = do
    b <- blah'
    putStrLn "Evaluated b"
    putStrLn b
    putStrLn b
    w <- woot
    putStrLn "Evaluated w"
    putStrLn w
    putStrLn w

-- Using the functor and applicative instances of IO
randomSum :: IO Int
randomSum = (+) <$> randomIO <*> randomIO

--  ===========================================================================
--                          Chapter Exercises
--  ===========================================================================
-- TODO: If I get around to writing the Vigenere cipher from the Algebraic datatypes
-- chapter, do this exercise.

-- Parse a directory with all INI files and get them in a Map FilePath Config
parseIniDir :: IO ()
parseIniDir = do
    putStr "Directory with ini files [ini-test-files]: "
    inp <- getLine
    let d = if not (null inp) then inp else "ini-test-files"
    iniFiles <- map (d </>) . filter (".ini" `isSuffixOf`) <$> getDirectoryContents d
    iniParses <-  traverse parseIniFile iniFiles
    let resultMap :: Map FilePath (Result Config)
        resultMap = M.fromList $ zip iniFiles iniParses
    print resultMap

parseIniFile :: FilePath -> IO (Result Config)
parseIniFile f = do
    c <- B.readFile f
    return $ parseByteString parseIni mempty c
