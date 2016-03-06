module Ch8 where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n =
    case n of
        0 -> "zero"
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        5 -> "five"
        6 -> "six"
        7 -> "seven"
        8 -> "eight"
        9 -> "nine"

digits' :: Int -> [Int]
digits' n
    | n > 9 = mod n 10 : digits' (div n 10)
    | otherwise = [n]

digits :: Int -> [Int]
digits = reverse . digits'

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

main = do
    print $ wordNumber 123456789
    print $ wordNumber 9876543210
