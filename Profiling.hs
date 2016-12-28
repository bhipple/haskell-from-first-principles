module Main where
import Control.Monad (replicateM_)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

blah :: [Integer]
blah = [1..1000]

f :: IO ()
f = print ([1..] !! 999999) >> putStrLn "f"

g :: IO ()
g = print ([1..] !! 9999999) >> putStrLn "g"

runtimeProfile :: IO ()
runtimeProfile = f >> g

main :: IO ()
main = do
    --runtimeProfile
    --heapAnalysis
    vectorAnalysis

heapAnalysis :: IO ()
heapAnalysis = replicateM_ 10000 (print blah)

-- For Int types, the Unboxed vector will use ~1/2 as much memory
-- as the more generic boxed vector.
vecInt = V.fromList [1..10000000] :: V.Vector Int
vecUnboxedInt = VU.fromList [1..10000000] :: VU.Vector Int

vectorAnalysis :: IO ()
vectorAnalysis = do
    let a = V.sum $ V.map (+1) vecInt
    let b = VU.sum $ VU.map (+1) vecUnboxedInt
    print a
    print b
