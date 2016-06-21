module Ch25_composing_types where
-- Composing Types
--
-- Unlike functors and applicatives, monads are not closed under composition.
--
-- Monad Transformers are never sum or identity types; they are written
-- as newtype wrappers that wrap one extra layer of monadic structure around a type.
newtype Identity a = Identity { runIdentity :: a }

newtype Compose f g a = Compose { getCompos :: f (g a) }
    deriving (Eq, Show)
