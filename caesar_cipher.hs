module Caesar where

import Data.Char

let2int :: Char -> Int
let2int c
    | isLower c = ord c - ord 'a'
    | otherwise = -1

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n + 26) `mod` 26)
    | isUpper c = toUpper $ shift n $ toLower c
    | otherwise = c

-- Takes a shift-size and encodes the string by shifting all letters
-- by the shift size (modulo alphabet size)
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- Decodes a string that has been encoded with a size-n cipher
decode :: Int -> String -> String
decode n = encode (-n)
