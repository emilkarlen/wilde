{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Utilities for Database IO
module Wilde.Media.Database.Monad
       (
         module Data.Convertible.Base,
         module Wilde.Media.Database,
         
         DatabaseMonad,
         ToDatabaseMonad(..),
         ToDatabaseError(..),
         throwErr,
         catchErr,
         
         runDatabase,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Control.Monad.Error.Class as ME

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Data.Convertible.Base

import Wilde.Media.Database


import qualified Wilde.Media.ElementSet as ES
import Wilde.Media.CustomEnvironment


-------------------------------------------------------------------------------
-- - DatabaseMonad -
-------------------------------------------------------------------------------


newtype DatabaseMonad a = DatabaseMonad (ExceptT DatabaseError (ReaderT ES.ElementSet IO) a)


instance ME.MonadError DatabaseError DatabaseMonad where
  throwError e = DatabaseMonad $ throwE e
  catchError (DatabaseMonad m) handler = DatabaseMonad $ catchE m handler'
    where
      handler' e =
        let
          (DatabaseMonad m) = handler e
        in
         m

instance Monad DatabaseMonad where
  return = DatabaseMonad . return
  (DatabaseMonad m) >>= f = DatabaseMonad $
                           do a <- m
                              let DatabaseMonad m' = f a
                              m'

instance Applicative DatabaseMonad where
  pure = DatabaseMonad . pure
  (DatabaseMonad ma) <*> (DatabaseMonad mb) = DatabaseMonad $ ma <*> mb

instance Functor DatabaseMonad where
  fmap f (DatabaseMonad m) = DatabaseMonad $ fmap f m

instance MonadIO DatabaseMonad where
  liftIO m = DatabaseMonad $ lift $ lift m

instance MonadWithCustomEnvironment DatabaseMonad where
  getCustomEnvironment = DatabaseMonad $ lift ask

runDatabase :: ES.ElementSet
            -> DatabaseMonad a
            -> IO (Either DatabaseError a)
runDatabase customEnvironment (DatabaseMonad m) = runReaderT (runExceptT m) customEnvironment

class ToDatabaseError a where
  toDatabaseError :: a -> DatabaseError

-- | Corresponds to 'Control.Monad.Trans.Error's throwError.
throwErr :: ToDatabaseError err
         => err -> DatabaseMonad a
throwErr err' = ME.throwError err
  where
    err = toDatabaseError err'

-- | Corresponds to 'Control.Monad.Trans.Error's catchError.
catchErr :: DatabaseMonad a                      -- ^ The computation that can throw an error.
            -> (DatabaseError -> DatabaseMonad a) -- ^ Error handler
            -> DatabaseMonad a
catchErr m handler = ME.catchError m handler

instance ToDatabaseError TranslationError where
  toDatabaseError = DbTranslationError

instance ToDatabaseError DatabaseError where
  toDatabaseError = id

instance ToDatabaseError ConvertError where
  toDatabaseError = DbTranslationError . AttributeTranslationError ""

instance ToDatabaseError e => ToDatabaseMonad (Either e) where
  toDatabaseMonad (Left err) = throwErr err
  toDatabaseMonad (Right x)  = return x

class ToDatabaseMonad m where
  toDatabaseMonad :: m a -> DatabaseMonad a

instance ToDatabaseError e => ToDatabaseMonad (ExceptT e IO) where
  toDatabaseMonad m =
    do
      res <- liftIO $ runExceptT m
      toDatabaseMonad res

instance ToDatabaseMonad TranslationMonad where
  toDatabaseMonad m =
    do
      custEnv <- getCustomEnvironment
      let eitherErrOk = runTranslation custEnv m
      toDatabaseMonad eitherErrOk

