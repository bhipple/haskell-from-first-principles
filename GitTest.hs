{-# LANGUAGE OverloadedStrings #-}
module GitTest where

import Data.Git
import Data.Git.Ref
import Data.Monoid
import Data.ByteString (ByteString)

import qualified Data.Set as S

{-- Someday, when I know more, come back to this library and see if I can figure it out}
-- Libgit and friends
import Git
import Git.Libgit2

wtf :: IO ()
wtf = withRepository lgFactory
        "/Users/bhipple/haskell-from-first-principles" $ do
    ref <- lookupReference "HEAD"
    case ref of
        Just (RefSymbolic name) -> print $ "Tracking branch " <> name
        _ -> print "Dunno man"
--}
--
-- Hit looks much simpler
gitDemo = do
    branches
    lastCommit

branches :: IO ()
branches = withRepo ".git" $ \git -> do
    heads <- branchList git
    mapM_ print heads

lastCommit :: IO ()
lastCommit = withRepo ".git" $ \git -> do
    ref <- resolveRevision git "HEAD"
    case ref of
        Nothing -> putStrLn "this sucks"
        Just r -> getCommit git r >>= print
