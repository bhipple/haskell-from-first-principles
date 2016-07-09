{-# LANGUAGE InstanceSigs #-}
module Ch26_monad_transformers where
-- Monad Transformers in practice.
--
-- MaybeT Transformer
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m)  where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))
    (MaybeT fab) <*> (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m) => Monad (MaybeT m) where
    return = pure
    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT ma) >>= f = MaybeT $ do
            v <- ma
            case v of
                Nothing -> return Nothing
                Just y -> runMaybeT (f y)


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT et) = EitherT $ (fmap . fmap) f et

instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT $ pure (pure x)
    (EitherT fab) <*> (EitherT ma) = EitherT $ (<*>) <$> fab <*> ma

instance Monad m => Monad (EitherT e m) where
    return = pure
    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT ma) >>= f = EitherT $ do
        v <- ma
        case v of
            (Left e) -> return $ Left e
            (Right a) -> runEitherT (f a)

swapEither :: Either a b -> Either b a
swapEither (Left a) = Right a
swapEither (Right b) = Left b

-- Transformer version of swapEither
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT v = EitherT $ swapEither <$> runEitherT v

-- Transformer version of either catamorphism
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fac fbc (EitherT amb) = do 
    v <- amb
    case v of
        (Left a) -> fac a
        (Right b) -> fbc b


