module Ch12 where
-- We use Maybe and Either to Signal Adversity and validate input.

-- Much stuff on kinds and * -> * vs. *, etc.

-- Neat trick: data constructors that take arguments behave like functions:
cool :: [Maybe Int]
cool = fmap Just [1, 2, 3]
