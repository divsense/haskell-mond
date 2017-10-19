module AppIO where

import Control.Monad.Trans
import Control.Monad.Trans.Except
import MondT

type AppIO = ExceptT String (MondT IO)

toNode :: NID -> AppIO Node
toNode nid = do x <- lift $ getNode nid
                case x of
                    Nothing -> throwE "Node not found"
                    Just r -> return r
