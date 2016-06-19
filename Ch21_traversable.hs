module Ch21 where
import Data.Functor.Identity
import qualified Data.Functor.Constant as C
import Data.ByteString.Lazy (ByteString)
import Network.Wreq

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Traversable
-- Allows transformation of elements inside a structure (like a functor),
-- producing applicative effects along the way, and lifts those potentially
-- multiple instances of applicative structure outside the traversable
-- structure.
-- Said differently, it traverses a structure mapping a function inside the
-- structure while accumulating the applicative contexts along the way.
-- Said even more simplistically, it swaps structures, turning something like
-- [Maybe] -> Maybe []
-- map is to fmap as mapM is to traverse and sequence is to sequenceA
--

{-- Typeclass Definition
class (Functor t, Foldable t) => Traversable t where
    {-# MINIMAL traverse | sequenceA #-}
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f

    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

-- LAWS
-- Traverse Function, if defined manually:
-- 1. Naturality
t . traverse f = traverse (t . f)

-- 2. Identity
traverse Identity = Identity

-- 3. Composition
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f

-- SequenceA Function, if defined manually:
-- 1. Naturality
t . seuqnceA = sequenceA . fmap t

-- 2. Identity
sequenceA . fmap Identity = Identity

-- 3. Composition
sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA
--}

urls :: [String]
urls = ["http://httpbin.com/ip", "http://httpbin.org/bytes/5"]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

-- Likely, we don't want a list of IO results; instead, we want an IO type
-- containing a list of results. Here's where traverse comes in
traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

-- Because traversable is stronger than Functor and Foldable, we can use
-- a traversable instance to define the weaker classes:
fmap' :: (Traversable t) => (a -> b) -> t a -> t b
fmap' f xs = runIdentity $ traverse (Identity . f) xs

-- And for foldMap:
foldMap' :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f t = C.getConstant $ traverse (C.Constant . f) t


--  ===========================================================================
--                          Chapter Exercises
--  ===========================================================================
newtype Identity' a = Identity' a
    deriving (Eq, Ord, Show)

instance Functor Identity' where
    fmap f (Identity' a) = Identity' (f a)

instance Foldable Identity' where
    foldMap f (Identity' a) = f a

instance Traversable Identity' where
    traverse f (Identity' a) = Identity' <$> f a

instance (Arbitrary a) => Arbitrary (Identity' a) where
    arbitrary = Identity' <$> arbitrary

instance (Eq a) => EqProp (Identity' a) where (=-=) = eq

q1 = quickBatch $ traversable (undefined :: Identity' (Int, Int, [Int]))


newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap f (Constant x) = Constant x

instance Foldable (Constant a) where
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    traverse f (Constant c) = Constant (f c)

checks = q1

-- TODO: Come back and do more exercises if I feel my intuition on traversable is lacking.
