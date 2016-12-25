{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
-- Monad Transformers in practice.
module Ch26_monad_transformers where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Monoid (mconcat)
import Data.Text.Lazy (Text)
import Web.Scotty
import qualified Data.Text.Lazy as TL

--  ===========================================================================
--                         MaybeT Transformer
--  ===========================================================================
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

--  ===========================================================================
--                         EitherT Transformer
--  ===========================================================================
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

--  ===========================================================================
--                         ReaderT Transformer
--  ===========================================================================
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma
    -- Equivalent?
    --fmap f (ReaderT rma) = ReaderT $ \r -> f <$> rma r

instance (Applicative m) => Applicative (ReaderT r m) where
    pure a = ReaderT (pure (pure a))
    (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
    return = pure
    (ReaderT rma) >>= f = ReaderT $ \r -> do
        a <- rma r
        runReaderT (f a) r

--  ===========================================================================
--                               StateT
--  ===========================================================================
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

-- We're writing the strict variant; the lazy variant will be demonstrated later
instance (Functor m) => Functor (StateT s m) where
    fmap f sm = StateT $ \s -> (\(a, s') -> (f a, s')) <$> runStateT sm s

instance (Monad m) => Applicative (StateT s m) where
    pure x = StateT $ \s -> pure (x, s)
    smf <*> sma = StateT $ \s -> do
        (f, s') <- runStateT smf s
        (a, s'') <- runStateT sma s'
        return (f a, s'')

instance (Monad m) => Monad (StateT s m) where
    return = pure
    StateT sma >>= f = StateT $ \s -> do
        (a, s') <- sma s
        runStateT (f a) s'

-- The Transformers library has a type that combines Reader, Writer, and State:
newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }

-- If we wanted to, we could define parsers as:
type Parser = StateT String Maybe

{-- We can use the Identity type to recover a normal type out of a transformer:
type Maybe a = MaybeT Identity a
--}
-- This can be used if we have a ReaderT, but only need a Reader, for instance.

-- When Haskellers say "base monad", they mean the structurally outermost monad:
type MyType a = IO [Maybe a]
-- here, base monad is IO

--  ===========================================================================
--                             MonadTrans
--  ===========================================================================
{--
class MonadTrans t where
    lift :: (Monad m) => m a -> t m a
--}

-- Motivation: putStrLn has type IO (), but it's inside the ActionM/ActionT monad
-- from Scotty.  If we want to use it in a do block, we need to lift it!
scotty1 = scotty 3000 $
    get "/:word" $ do
        beam <- param "word"
        (lift :: IO a -> ActionM a) (putStrLn "hello")
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

param' :: Parsable a => Text -> ExceptT String ActionM a
param' k = ExceptT $
    rescue (Right <$> param k)
           (const (return (Left $ "The key: " ++ show k ++ " was missing!")))

type Reco = (Integer, Integer, Integer, Integer)

tshow = TL.pack . show

-- Try this one with:
-- http://localhost:3000/?1=1
-- http://localhost:3000/?1=1&2=2&3=3&4=4
scotty2 = scotty 3000 $
    get "/" $ do
        reco <- runExceptT $ do
                    a <- param' "1"
                    liftIO $ print a
                    b <- param' "2"
                    c <- param' "3"
                    d <- param' "4"
                    (lift . lift) $ print b
                    return ((a, b, c, d) :: Reco)
        case reco of
            (Left e) -> text (TL.pack e)
            (Right r) -> html $
                mconcat [ "<h1>Success! Reco was: ", tshow r, "<h1>" ]


-- TODO: Do all of the Monad Transformers chapter exercises
