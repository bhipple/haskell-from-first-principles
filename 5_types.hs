module ChTypes where

-- Type systems eliminate the need to write tests
-- that do nothing except verify that we're passing the right
-- sort of data in and out of functions.

f :: Num a => a -> a -> a
f x y = x + y + 3

-- The compiler will infer g to have the same type as f
-- If we apply an x or y that's fractional, :t will tell us that
-- the expression has type Fractional a => a -> a
g x y = x + y + 3

-- Type filling exercises
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w . q $ a

-- Ex 3
data X
data Y
data Z

xz :: X -> z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x,y) = (xz x, yz y)
