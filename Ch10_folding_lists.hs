module Ch10 where

-- A fold is known as a catamorphism
--
-- Fun function to show how fold associates
demo = map show [1..5]
showAssoc :: String -> String -> String
showAssoc x y = concat ["(", x, "+", y, ")"]

rightDemo = foldr showAssoc "0" demo
leftDemo = foldl showAssoc "0" demo

-- Folding happens in two stages: traversal and folding.

-- Chapter Exercies
stopVowelStop :: String -> String -> [String]
stopVowelStop stops vowels = [[a] ++ [b] ++ [c] | a <- stops, b <- vowels, c <- stops]

nounVerbNoun :: [String] -> [String] -> [(String, String, String)]
nounVerbNoun n v = [(n1, v1, n2) | n1 <- n, v1 <- v, n2 <- n]

ex1a = stopVowelStop "pbtdkg" "aeiou"
ex1b = filter ((== 'p') . head) ex1a
ex1c = nounVerbNoun ["runner", "ball", "bat"] ["hit", "run", "score"]

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> b || f a) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x : xs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x xs -> if f x then x : xs else xs) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x xs -> f x ++ xs) []

squishAgain :: [[a]] -> [a]
squishAgain = undefined
