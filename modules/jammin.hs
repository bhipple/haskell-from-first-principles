module Jammin where

import Data.List

data Fruit =
    Apple
  | BlackBerry
  | Peach
  | Plum
  deriving (Eq, Ord, Show)

data JamJar = JamJar {
    fruit :: Fruit,
    jars :: Int }
    deriving (Eq, Ord, Show)

row1 = JamJar Apple 5
row2 = JamJar Peach 2
row3 = JamJar Plum 8
row4 = JamJar BlackBerry 15
row5 = JamJar Apple 7
allJam = [row1, row2, row3, row4, row5]

totalJam :: [JamJar] -> Int
totalJam = foldr ((+) . jars) 0

largestRow :: [JamJar] -> JamJar
largestRow = maximum
--largestRow = foldr1 max -- also works, but hlint suggests maximum!

compareKind :: JamJar -> JamJar -> Ordering
compareKind (JamJar k _) (JamJar k' _) = compare k k'

sortRows :: [JamJar] -> [JamJar]
sortRows = sortBy compareKind

sameType :: JamJar -> JamJar -> Bool
sameType x y = fruit x == fruit y

groupJam :: [JamJar] -> [[JamJar]]
groupJam = groupBy sameType . sortRows
