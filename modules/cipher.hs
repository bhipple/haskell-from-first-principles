module Cipher where

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


-- ============================================================================
--                              Vigenere Cipher
-- ============================================================================
type Keyword = String

-- The mask character that will have no effect on encoding
emptyMask :: Char
emptyMask = 'a'

-- Given a (msg, kw) pair, shift the msg char by (kw - 'a') slots
shiftV :: (Char, Char) -> Char
shiftV (m, k) = shift shiftSize m
    where shiftSize = let2int k

-- When encountering a space, zip in the emptyMask instead of consuming from the 
-- keyword mask buffer.
zipExceptSpaces :: Keyword -> String -> [(Char, Char)]
zipExceptSpaces _ [] = []
zipExceptSpaces (k:ks) (m:ms) = if m == ' ' then (m, emptyMask) : zipExceptSpaces (k:ks) ms
                                  else (k, m) : zipExceptSpaces ks ms

encodeV :: Keyword -> String -> String
encodeV kw msg = map shiftV $ zipExceptSpaces (mconcat . repeat $ kw) msg

testV1 :: IO ()
testV1 = if res == "mppr ae oywy"
            then putStrLn "Success!"
            else putStrLn $ "Fail: received " ++ res
        where res = encodeV "ally" "meet at dawn"
