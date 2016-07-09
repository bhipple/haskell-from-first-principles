{-# LANGUAGE InstanceSigs #-}
module Ch25_composing_types where
-- Composing Types
import Data.Bifunctor
--
-- Unlike functors and applicatives, monads are not closed under composition.
--
-- Monad Transformers are never sum or product types; they are written
-- as newtype wrappers that wrap one extra layer of monadic structure around a type.
newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Show)

-- As with function composition, but f and g are type constructors
-- Compose :: (* -> *) -> (* -> *) -> * -> *
newtype Compose f g a = Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

-- If both f and g have functor definitions, we can define one for Compose:
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

nada2 = fmap (+1) (Compose [Nothing, Just 1])

-- We can nest this to arbitrary depth, because functors are closed under composition:
v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]

-- Likewise, applicatives are closed under composition
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure x = Compose $ (pure . pure) x

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    Compose f <*> Compose a = Compose $ (<*>) <$> f <*> a
    -- ^ We first fmap the <*> into f, then combine the outer structures. Because the
    -- inner structure of f is now an applicative application function, it combines nicely
    -- with the inner structure of a to produce the desired result.

pure1 :: Compose Maybe [] Integer
pure1 = pure 1

purePlus1 :: Compose Maybe [] (Integer -> Integer)
purePlus1 = pure (+1)

composedJust2 = purePlus1 <*> pure 1

-- However, it's not possible to do the same for Monads
-- No way to write this:
instance (Monad f, Monad g) => Monad (Compose f g) where
    return = pure

    (>>=) :: Compose f g a
          -> (a -> Compose f g b)
          -> Compose f g b
    (>>=) = undefined
{-- because we can't reconcile the type differences:
Monad f => f a -> (a -> f b) -> f b
Monad g => g a -> (a -> g b) -> g b

-- i.e., binding:
(Monad f, Monad g) => f (g a) -> (a -> f (g b)) -> f (g b)
-- or
(Monad f, Monad g) => f (g (f (g a))) -> f (g a)
--}
-- Because we can't compose any two arbitrary monads, we use helpers:  monad transformers

-- Here, we have to foldmap the inner type,
-- and then foldmap the outer type to reduce everything
-- TODO: Figure this one out
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap = undefined
    -- foldMap f a = ((fmap . fmap) foldMap f a)
    --                   (Compose r) = fmap foldMap f g'
    --               in r

-- TODO: Traversable instance
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse = undefined

-- Bifunctor instances
data Deux a b = Deux a b

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a

instance Bifunctor Const where
    bimap f g (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap f g (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
    bimap f g (Left' l) = Left' $ f l
    bimap f g (Right' r) = Right' $ g r


-- A monad transformer is a type constructor that takes a Monad as an argument
-- and returns a Monad as a result. We can get a generic transformer for a specific
-- monad type and one unknown monad type, which prevents us from having to write a
-- function for each combination of any two monad types.
--
-- With Identity, we can specify an IdentityT monad transformer:
newtype IdentityT f a = IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

-- We know nothing about the structure holding the values in the IdentityT,
-- except that it's a functor. This allows us to fmap inside
instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)
    (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
    return = pure
    (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f

ex1 = IdentityT [1,2,3] >>= (return . (+1))

-- In the transformers library, it's written:
-- m >>= k = IdentityT $ runIdentityT . k =<< runIdentityT m
