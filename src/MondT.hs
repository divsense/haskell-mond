module MondT
    ( MondT(..), Model, Node(..), NID, PID, Tag, Prop, Text, Path
    ) where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read hiding (step)

type NID = String
type PID = String
type Tag = String
type Text = String
type Path = String
type Prop = (String,String)
type Model = Map NID Node

data Node = Node NID PID Tag Text [Prop] [NID] deriving Show

newtype MondT m a = MondT { runMond :: Model -> m (a, Model) }

instance Monad m => Functor (MondT m) where
    -- fmap :: (a -> b) -> MondT m a -> MondTn m b
    fmap f mma = MondT $ \model -> do (a, model') <- (runMond mma) model
                                      return ((f a), model')

instance Monad m => Applicative (MondT m) where
    -- pure :: a -> MondT m a
    pure a = MondT $ \model -> pure (a, model)

    -- (<*>) :: MondT m (a -> b) -> MondT m a -> MondT m b
    mmf <*> mma = MondT $ \model -> do (f, _) <- (runMond mmf) model
                                       (a, model') <- (runMond mma) model
                                       return ((f a), model')

instance Monad m => Monad (MondT m) where
    return = pure

    -- (>>=) :: MondT m a -> (a -> MondT m a) -> MondT m b
    mma >>= f = MondT $ \model -> do (a, model') <- (runMond mma) model
                                     runMond (f a) $ model'


