module Ch3 where

-- Not using !! here for exercise
thirdLetter :: String -> Char
thirdLetter x = head . take 1 $ drop 2 x

letterIdx :: String -> Int -> Char
letterIdx s x = head . take 1 $ drop (x-1) s
