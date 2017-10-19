module EitherMondT where

import MondT
import MEitherT

newtype EitherMondT e m a = EitherMondT { runEMT :: MEitherT e (MondT m) a }

instance Monad m => Functor (MondT m) where
    fmap :: (a -> b) -> MondT m a -> MondTn m b
    fmap f mma = MondT $ \model -> do (a, model') <- (runMondT mma) model
                                      return ((f a), model')

instance Monad m => Applicative (MondT m) where
    pure :: a -> MondT m a
    pure a = MondT $ \model -> pure (a, model)

    (<*>) :: MondT m (a -> b) -> MondT m a -> MondT m b
    mmf <*> mma = MondT $ \model -> do (f, _) <- (runMondT mmf) model
                                       (a, model') <- (runMondT mma) model
                                       return ((f a), model')

instance Monad m => Monad (MondT m) where
    return = pure

    (>>=) :: MondT m a -> (a -> MondT m a) -> MondT m b
    mma >>= f = MondT $ \model -> do (a, model') <- (runMondT mma) model
                                     runMondT (f a) $ model'


unsafeNode :: Monad m => NID -> MondT m Node
unsafeNode nid = MondT $ \model -> let node = model ! nid
                                   in return (node, model)

