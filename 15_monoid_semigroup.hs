module Ch15 where

import Data.Monoid

{--
-- A Monoid is a binary associative operation with an identity

-- Laws for Monoids
-- Left and Right Identity
mappend mempty x = x
mappend x mempty = x

-- Associativity
mappend x (mappend y z) = mappend (mapend x y) z

mconcat = foldr mappend mempty
-}

-- Maybe has many valid monoids, two of which are First and Last
just3 = First Nothing <> First (Just 3) <> First (Just 5) <> First Nothing
just5 = Last Nothing <> Last (Just 3) <> Last (Just 5) <> Last Nothing
nothin = First Nothing <> First Nothing

-- Practice writing a Monoid Instance
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada Nada = Nada
    mappend (Only x) Nada = Only x
    mappend Nada (Only x) = Only x
    mappend (Only x) (Only y) = Only (mappend x y)


-- Mad Lib Example
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
    e <> "! he said " <> adv <> " as he jumped into his car "
    <> noun <> " and drove off with his " <> adj <> " wife."

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' e adv noun adj =
    mconcat [e, "! he said ", adv, " as he jumped into his car ", noun,
             " and drove off with his ", adj, " wife."]
