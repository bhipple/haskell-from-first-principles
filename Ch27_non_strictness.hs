{-# LANGUAGE BangPatterns #-}
module Ch27_non_strictness
where

import Debug.Trace (trace)

--- Non-Strictness in Haskell
-- * Call-by-name and call-by-need evaluation
-- * Analyzing runtime behavior in terms of sharing
-- * Analyzing runtime behavior in terms of efficiency
--
-- In general, any implementation of non-strictness is acceptable as long
-- as it respects when it's supposed to return a value successfully or bottom
-- out, i.e. `snd (undefined, 1)` must return 1 successfully.

-- Forcing strictness with seq
-- seq will force evaluation of the first argument to weak head normal form
-- when the second argument has to be evaluated. Note that if the b is never
-- evaluated, neither will the a!
seq' :: a -> b -> b
seq' = undefined
-- This works by evaluating arguments as case statements in CORE.
-- To see output in the core language, try this in GHCI:
-- :set -ddump-simpl -dsuppress-all
-- :l my_file.hs

-- Thunks
-- Almost everything gets thunked until evaluated, except for trivial
-- data constructors. Take a look at this with the :sprint command:
-- (Note you may need to run it with `let l1 = [1,2,3] :: [Integer]`
-- in ghci to get the effect.
l1 :: [Integer]
l1 = [1,2,3]

-- Not evaluated because it has the Num type constructor
l2 = [1,2,3]

-- The `id 3` gets thunked, while the rest is evaluated
l3 :: [Integer]
l3 = [1,2,id 3, 4] :: [Integer]


-- Sharing
-- GHC will switch between call-by-name and call-by-need based on necessity
-- and what it thinks will produce faster code.
-- Use Debug.Trace to set traces on values and see when they're evaluated!
inc = (+1)
twice = inc . inc

-- Here we can see that the second trace gets shared inside the twice
howManyTimes = inc (trace "I got eval'd" (1+1))
                   + twice (trace "I got eval'd" (1+1))

-- Here, everything shares the value because we named it
-- If we call either of these functions again in a repl, it'll evaluate
-- zero times.
howManyTimes' =
    let onePlusOne = trace "I got eval'd" (1+1)
     in inc onePlusOne + twice onePlusOne

-- Names and values are shared, but the results of calling
-- functions with specific arguments are NOT.  E.g., call this twice
-- with the same a in a repl and you'll get it executed each time:
f :: a -> Int
f _ = trace "f" 1

-- Note that the eta reduction actually changes the sharing properties!
-- This function evaluates once and then shares results
g :: a -> Int
g = trace "g" const 1

-- This function fully evaluates every time
g' :: a -> Int
g' x = trace "g'" const 1 x

-- Typeclass constraints inhibit sharing as well, because in Core they desugar to functions
-- awaiting type arguments. Under the hood, a polymorphic value that's been evaluated
-- is still a function, not a value! Polymorphic functions can't be reduced in the same
-- way that this function blah can't be reduced, since they may be called differently:
blah :: Num a => a -> a
blah x = x + 1

-- If you run this once and then sprint it, you'll see it's just the value 2.
concrete :: Int
concrete = blah 1

-- Preventing Sharing On Purpose
-- If we have a large intermediate result that we don't want stored in memory until
-- a GC pass, we can intentionally stop sharing with some techniques.
shares x = x + x

-- Computes value of X once inside sharesX, then shareDemo is POD.
-- No matter which of these is forced first, the calculation is only done once.
sharing = shares (trace "hi" 2)
shareDemo = sharing
shareDemo' = sharing

-- By turning x into a simple unit function, it can't be shared, so we'll see hi twice
doesntShare x = x () + x ()

noShareDemo = doesntShare (\_ -> trace "hi" 2)

-- Forcing Sharing
-- As said earlier, the best way to force sharing is to give something a name,
-- usually with a let binding. For instance, the implementation of forever:
forever' :: (Monad m) => m a -> m b
forever' a = let a' = a >> a' in a'
-- The let binding turns on sharing, which causes GHC to overwrite the thunk
-- at each step in the evaluation, instead of indefinitely constructing new thunks
-- and causing a memory space-leak.

-- Rule of thumb: lazy in the spine, strict in the leaves!
data BangBang = SheShotMeDown !Int !String
gimme :: BangBang -> String
gimme (SheShotMeDown _ s) = s

bottom = gimme (SheShotMeDown undefined "foobar")

-- To default everything to strict on a per-module basis in GHC 8, use:
-- LANGUAGE Strict, and
-- LANGUAGE StrictData
-- If these pragmas are on, everything has an implicit ! or seq by default.
-- To recover laziness in certain places, use ~
--

--  ===========================================================================
--                          Chapter Exercises
--  ===========================================================================
-- sprint output
-- 1. let x = 1 => _
-- 2. let x = ['1'] => "1"
-- 3. let x = [1] => _
-- 4. let x = 1 :: Int => 1
-- 5. let f = \x -> x
--    let x = f 1 => _
-- 6. let f :: Int -> Int; f = \x -> x
--    let x = f 1 => _

-- Will printing result in bottom
e1 = snd (undefined, 1) -- No
e2 = let x = undefined
         y = x `seq` 1
      in snd (x, y) -- yes
e3 = length $ [1..5] ++ undefined -- yes, becausee the undefined is a list not a num!
e4 = length $ [1..5] ++ [undefined] -- no, just need the spine not the value
e5 = const 1 undefined -- no
e6 = const 1 (undefined `seq` 1) -- no
e7 = const undefined 1 -- yes

-- Make the expression bottom using only bang patterns or seq
b1 = let !x = undefined
         y = "blah"
      in print (snd (x,y))
