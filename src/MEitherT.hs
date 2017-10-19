module MEitherT where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype MEitherT e m a = MEitherT { runEitherT :: m (Either e a) }

instance Monad m => Functor (MEitherT e m) where
    -- fmap :: (a -> b) -> MEitherT m a -> EitherTn m b
    fmap f ema = MEitherT $ do e <- (runEitherT ema)
                               return (fmap f e)
                              
instance Monad m => Applicative (MEitherT e m) where
    -- pure :: a -> MEitherT e m a
    pure a = MEitherT $ pure (Right a)

    -- (<*>) :: MEitherT e m (a -> b) -> MEitherT e m a -> MEitherT e m b
    emf <*> ema = MEitherT $ do ef <- runEitherT emf
                                ea <- runEitherT ema
                                return (ef <*> ea)

instance Monad m => Monad (MEitherT e m) where
    return = pure

    -- (>>=) :: MEitherT e m a -> (a -> MEitherT e m a) -> MEitherT e m b
    ema >>= f = MEitherT $ do e <- runEitherT ema
                              case e of
                                Left l -> return (Left l)
                                Right r -> runEitherT (f r)

instance MonadTrans (MEitherT e) where
    --lift :: m a -> MEitherT m a
    lift m = MEitherT $ do a <- m
                           return (Right a)

instance (MonadIO m) => MonadIO (MEitherT e m) where
    liftIO m = lift (liftIO m)

mfail :: Monad m => e -> MEitherT e m a
mfail = MEitherT . return . Left


