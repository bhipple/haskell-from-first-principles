module Ch28_basic_libraries
where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Vector ((//))

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic.Mutable as GM

-- Basic Libraries and Data Structures.
-- Chapter goals:
-- * Demonstrate how to measure time and space usage
-- * Offer guidelines on WHNF vs NF for benchmarking
-- * Define constant applicative forms and argument saturation
-- * Analyze different data structures for different circumstances

--  ===========================================================================
--                     Benchmarking with Criterion
--  ===========================================================================
--  Writing a total version of !! that returns maybes instead of bottoms.
infixl 9 !?
_ !? n | n < 0 = Nothing
[] !? _ = Nothing
(x : _) !? 0 = Just x
(_ : xs) !? n = xs !? (n - 1)

-- See Benchmarks.hs for a Criterion benchmark of this vs. the regular !!

-- Since our implementation above is equivalent to the naive Haskell Report
-- implementation, it's significantly slower than the optimized foldr version
-- currently in base.
infixl 9 !??
{-# INLINABLE (!??) #-}
(!??) :: [a] -> Int -> Maybe a
xs !?? n
    | n < 0 = Nothing
    | otherwise = foldr (\x r k -> case k of
                                     0 -> Just x
                                     _ -> r (k-1)) (const Nothing) xs n
-- This runs as quickly as the standard !!

--  ===========================================================================
--                             WHNF vs NF
--  ===========================================================================
-- WHNF evaluates up to the first data constructor, while NF fully evaluates.
-- E.g., if we have Maybe a, it'll evaluate the Maybe enough to see if it's a Just
-- or a Nothing, but won't evaluate the a.
-- Use WHNF when the first data constructor is a meaningful indicator of whether the work
-- you're interested in has been done.

-- If we have Guarded Recursion (recursion with a data constructor between each step),
-- WHNF will do nothing. For instance, something like (map (+1) [0..10000]) constructs
-- cons cells as it goes through the list. WHNF will bench to a few nanoseconds because
-- it just thunks the data constructor instead of doing real work; doesn't even traverse
-- the list. Try it with `bench "Too lazy!" $ whnf (map (+1) [undefined, undefined, undefined])`

--  ===========================================================================
--                              Profiling
--  ===========================================================================
-- While benchmarking is about getting raw speed numbers, profiling is about figuring
-- out where a program is spending its time percentage-wise.

-- See Profiling.hs for an example
-- To generate profiling data, run:
-- $ stack ghc -- -prof -fprof-auto -rtsopts -O2 Profiling.hs
-- $ ./Profiling +RTS -P
-- $ cat Profiling.prof
--
-- The flags:
-- -prof: Enables profiling in the first place, which slightly slows down programs
--        By default, cost centers (SCCs) have to be annotated manually.
-- -fprof-auto: All bindings not marked inline automatically get a profiling cost center assigned
-- -rtsopts: Lets you pass RTS options to the compiled binary, rather than compiling them into the binary. Results in a smaller binary.

-- We can also do memory analysis using:
-- $ stack ghc -- -prof -fprof-auto -rtsopts -O2 Profiling.hs
-- $ ./Profiling +RTS -hc -p
-- $ hp2ps Profiling.hp
-- $ open Profiling.ps

--  ===========================================================================
--                      Constant Applicative Form
--  ===========================================================================
-- Constant Applicative Forms (CAFs) are expressions that have no free variables
-- and are held in memory to be shared with all other expressions in a module.
-- Includes:
-- * Values
-- * Partially applied functions without named arguments
-- * Fully applied functions (like a fib list) (rare in real code)
--
-- CAFs cause sharing, which can potentially speed up programs -- or slow them down,
-- since they're stored on the heap. Use profiling techniques from above to find them
-- and deal with them accordingly.

--  ===========================================================================
--                           Data Structures
--  ===========================================================================
-- Map and Set
-- Note that if your key is an int, you're probably better off with HashMap,
-- IntMap, or Vector.

-- Sequence
-- Buit on finger trees, this is a list that gives cheap appends to be either end
{--
newtype Seq a = Seq (FingerTree (Elem a))
newtype Elem a = Elem { getElem :: a }

data FingerTree a
  = Empty
  | Single a
  | Deep {-# UNPACK #-} !Int !(Digit a)
         (FingerTree (Node a)) !(Digit a)
--}
-- Aside from cheap appends to the end, sequence also has faster lookup access to the tail.
-- Generally, List is fairly competitive with Sequence, so don't use it by defualt.

-- Vector
-- In addition to all the normal nice reasons to use a vector,
-- they also support efficient slicing (creating a "new" vector out of a [start,end] range)

-- Slicing for lists (vector has its own V.slice function)
sliceList :: Int -> Int -> [a] -> [a]
sliceList from len xs = take len (drop from xs)

-- They also have stream fusion to avoid multiple passes and intermediate data structures
manuallyFused :: Int -> V.Vector Int
manuallyFused n = V.map ((+n) . (+n) . (+n) . (+n)) $ V.fromList [1..10000]

compilerFused :: Int -> V.Vector Int
compilerFused n = V.map (+n) $ V.map (+n) $ V.map (+n) $ V.map (+n) $ V.fromList [1..10000]

-- Batch update operator //
-- This takes a list of [(idx, elem)] pairs and updates the vector at each idx to have
-- the value elem in that idx.
vec :: V.Vector Int
vec = V.fromList [1..10000]

-- This will replace the first n elements with the value 0,
-- but it doesn't do the batch operation in batch!
slow :: Int -> V.Vector Int
slow n = go n vec
    where go 0 v = v
          go n v = go (n-1) (v // [(n, 0)])

-- 500-1000x faster, thanks to batching optimizations
batchList :: Int -> V.Vector Int
batchList n = vec // updates
    where updates = fmap (\n -> (n, 0)) [0..n]

-- ~1.4x faster, by using a Vector
batchVector :: Int -> V.Vector Int
batchVector n = V.unsafeUpdate vec updates
    where updates = fmap (\n -> (n, 0)) $ V.fromList [0..n]

--  ===========================================================================
--                              Mutation
--  ===========================================================================
-- Mutable Vectors: for when the above just isn't fast enough

-- >7000x faster than the original slow update!
mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
    mvec <- GM.new (n+1)
    go n mvec
    where go 0 v = return v
          go n v = MV.write v n 0 >> go (n-1) v

-- ~1.5x slower than the mutable one, due to the ST monad freeze conversion
-- into a regular V
mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
    mvec <- GM.new (n+1)
    go n mvec
        where go 0 v = V.freeze v
              go n v = MV.write v n 0 >> go (n-1) v

--  ===========================================================================
--                          Strings and Text
--  ===========================================================================
-- Text encodes in UTF-16.
-- Don't use ByteString.Char8; it's there for ASCII-only data, and you usually
-- want Unicode with ByteString.UTF8.

--  ===========================================================================
--                          Chapter Exercises
--  ===========================================================================
--  A Difference List is a simpler Sequence data structure that specifically
--  addresses the slow appending problem.
--  cons, snoc, and append all take O(1) time. toList takes O(n) time, of course.
--  We do this through function composition.
newtype DList a = DL { unDL :: [a] -> [a] }

{-# INLINE empty #-}
empty :: DList a
empty = DL id

{-# INLINE singleton #-}
singleton :: a -> DList a
singleton = DL . (:)

-- Type note: ($[]) :: ([a] -> b) -> b generally, but in this case it's
-- ($[]) :: ([a] -> [a]) -> [a]
{-# INLINE toList #-}
toList :: DList a -> [a]
toList = ($[]) . unDL

{-# INLINE cons #-}
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x:) . unDL xs)

{-# INLINE snoc #-}
infixr `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ unDL xs . (x:)

{-# INLINE append #-}
append :: DList a -> DList a -> DList a
append xs ys = DL $ unDL xs . unDL ys


schlemiel :: Int -> [Int]
schlemiel i = go i []
    where go 0 xs = xs
          go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
    where go 0 xs = xs
          go n xs = go (n-1) (singleton n `append` xs)

-----------------
-- A simple Queue
data Queue a = Queue { enqueue :: [a]
                     , dequeue :: [a]
                     } deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push x (Queue e d) = Queue (x:e) d

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue e d) = case (e, d) of
    ([], []) -> Nothing
    (e, []) -> pop (Queue [] (reverse e))
    (e, d:ds) -> Just (d, Queue e ds)
