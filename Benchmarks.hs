module Main where

import Criterion.Main
import Ch28_basic_libraries
import qualified Data.Vector as V

myList :: [Int]
myList = [1..9999]
myList' = myList ++ [undefined]

whnfVsNFBenchmarks :: [Benchmark]
whnfVsNFBenchmarks =
    [ bench "base !! 9998"
        $ whnf (myList !!) 9998
    , bench "naive recursive !? 9998"
        $ whnf (myList !?) 9998
    , bench "optimized foldr !?? 9998"
        $ whnf (myList !??) 9998
    , bench "whnf !?? on undefined is OK thanks to Just data constructor"
        $ whnf (myList' !??) 9999
    -- , bench "whnf !! on undefined is NOT OK"
    --     $ whnf (myList' !!) 9999
    -- , bench "nf on undefined is NOT OK"
    --     $ nf (myList' !??) 9999
    --
    -- Cases where WHNF is insufficient
    , bench "whnf does nothing here"
        $ whnf (map (+1)) myList
    , bench "whnf too lazy thanks to data constructor of cons cell at first element"
        $ whnf (map (+1)) [undefined, undefined, undefined]
    , bench "nf runs the actual map"
        $ nf (map (+1)) myList
    ]


lst :: [Int]
lst = [1..1000]

v :: V.Vector Int
v = V.fromList lst

vectorBenchmarks :: [Benchmark]
vectorBenchmarks =
    [ bench "slicing list" $
        whnf (head . sliceList 100 900) lst
    , bench "slicing vector is much faster thanks to views" $
        whnf (V.head . V.slice 100 900) v
    , bench "vector map manually fused" $
        whnf manuallyFused 9998
    , bench "vector map compiler fused" $
        whnf compilerFused 9998
    , bench "slow // updates" $
        whnf slow 9998
    , bench "batch list // updates" $
        whnf batchList 9998
    , bench "batch vector // updates" $
        whnf batchVector 9998
    , bench "mutable IO vector" $
        whnfIO (mutableUpdateIO 9998)
    , bench "mutable ST vector" $
        whnf mutableUpdateST 9998
    ]

main :: IO ()
main = defaultMain $ []
     -- ++ whnfVsNFBenchmarks
        ++ vectorBenchmarks
