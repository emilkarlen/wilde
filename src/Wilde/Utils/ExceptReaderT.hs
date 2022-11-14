module Wilde.Utils.ExceptReaderT where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

withEnv :: (env -> env) -> ExceptT err (ReaderT env m) a -> ExceptT err (ReaderT env m) a
withEnv = mapExceptT . local
