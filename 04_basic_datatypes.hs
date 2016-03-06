module Ch4 where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool coolness
        then putStrLn "eyyyy. What's shakin'?"
    else
        putStrLn "pshhhh."
    where cool v = v == "downright frosty yo"

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f x y = ((snd x, snd y), (fst x, fst y))
