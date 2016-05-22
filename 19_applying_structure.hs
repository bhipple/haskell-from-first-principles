module Ch19 where
import qualified Data.Map as M
import Data.Monoid
-- Numerous examples of using monoids, functors, applicatives, and monads in real Haskell code.
--
{-- Monoids of functions:
instance Monoid b => Monoid (a -> b) where
    mempty _ = mempty
    mappend f g = \a -> (f a) <> (g a)
--}

f = M.fromList [('a', 1)]
g = M.fromList [('b', 2)]
ex = f <> g

-- Chapter contains a nice example of using Scotty and Redis to make a simple
-- webpage for entering, saving, and looking up information.
