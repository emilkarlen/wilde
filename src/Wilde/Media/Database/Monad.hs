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

-- | A monad that operates on a single db connection.
--
-- The monad is able to render SQL statements (using the
-- configured sql renderer) and to execute such sql statements.
--
-- Connecting and disconnecting from the db is handled
-- outside this monad.
--
-- The db connection is hidden, so there is
-- no way to close the connection from within this monad.
module Wilde.Media.Database.Monad
      (
        module Data.Convertible.Base,
        module Wilde.Media.Database,
        
        Monad(..),
        ToDatabaseError(..),
        ToMonad(..),
        Environment,
        newEnv,
        -- * Running and error handling
        run,
        throwErr,
        catchErr,
        -- * Wrapping
        inTransaction,
        -- * SQL execution
        prepareSql,
        prepareSql_str,
        -- * Logging
        Logging.MonadWithLogging(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Prelude hiding (Monad)
import qualified System.IO as IO
import qualified Control.Monad.Error.Class as ME
import qualified Control.Monad as MMonad

-- LOGGING begin
-- import qualified System.IO as IO
-- LOGGING end

import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader

import Data.Convertible.Base

import qualified Database.HDBC as HDBC
-- prepare :: IConnection conn => conn -> String -> IO Statement
-- execute :: Statement -> [SqlValue] -> IO Integer

import Wilde.Media.Database
import qualified Wilde.Utils.Logging as Logging
import Wilde.Media.Database.Error

import Wilde.Database.Sql

import qualified Wilde.Media.ElementSet as ES
import Wilde.Media.CustomEnvironment
import Wilde.Database.DmlRenderer (DmlRenderer)


-------------------------------------------------------------------------------
-- - monad environment -
-------------------------------------------------------------------------------


data Environment = Environment
  {
    envCustEnv     :: ES.ElementSet
  , envSqlRenderer :: SqlDmlStatement SqlIdentifier -> String
  , envDbConn      :: HDBC.ConnWrapper
  }

-- | Constructor of `Environment`.
--
-- Needed since the constructors of `Environment` are hidden.
newEnv :: ES.ElementSet -- ^ custom environment
       -> (SqlDmlStatement SqlIdentifier -> String) -- ^ SQL renderer
       -> HDBC.ConnWrapper -- ^ db connection
       -> Environment
newEnv = Environment


-------------------------------------------------------------------------------
-- - monad -
-------------------------------------------------------------------------------


newtype Monad a = Monad (ExceptT DatabaseError (ReaderT Environment IO) a)


instance ME.MonadError DatabaseError Monad where
  throwError e = Monad $ throwE e
  catchError (Monad m) handler = Monad $ catchE m handler'
    where
      handler' e =
        let
          (Monad m) = handler e
        in
         m

instance MMonad.Monad Monad where
  (Monad m) >>= f = Monad $
                            do a <- m
                               let Monad m' = f a
                               m'

instance Applicative Monad where
  pure = Monad . pure
  (Monad ma) <*> (Monad mb) = Monad $ ma <*> mb

instance Functor Monad where
  fmap f (Monad m) = Monad $ fmap f m

instance MonadIO Monad where
  liftIO m = Monad $ lift $ lift m


-------------------------------------------------------------------------------
-- - getting from the environment -
-------------------------------------------------------------------------------


getEnv :: (Environment -> a) -> Monad a
getEnv getter = Monad $ lift $ asks getter


-------------------------------------------------------------------------------
-- - instances of "monad interfaces" -
-------------------------------------------------------------------------------


instance MonadWithCustomEnvironment Monad where
  getCustomEnvironment = getEnv envCustEnv


instance Logging.MonadWithLogging Monad where
  logg = do_logg


-------------------------------------------------------------------------------
-- - monad execution -
-------------------------------------------------------------------------------


run :: Environment
    -> Monad a
    -> IO (Either DatabaseError a)
run environment (Monad m) = runReaderT (runExceptT m) environment

-- | Corresponds to 'Control.Monad.Trans.Error's throwError.
throwErr :: ToDatabaseError err
         => err
         -> Monad a
throwErr err' = ME.throwError err
  where
    err = toDatabaseError err'

-- | Corresponds to 'Control.Monad.Trans.Error's catchError.
catchErr :: Monad a                    -- ^ The computation that can throw an error.
         -> (DatabaseError -> Monad a) -- ^ Error handler
         -> Monad a
catchErr m handler = ME.catchError m handler


-------------------------------------------------------------------------------
-- - ToMonad -
-------------------------------------------------------------------------------


class ToMonad m where
  toMonad :: m a -> Monad a

instance ToDatabaseError e => ToMonad (Either e) where
  toMonad (Left err) = throwErr err
  toMonad (Right x)  = return x

instance ToDatabaseError e => ToMonad (ExceptT e IO) where
  toMonad m =
    do
      res <- liftIO $ runExceptT m
      toMonad res

instance ToMonad TranslationMonad where
  toMonad m =
    do
      custEnv <- getCustomEnvironment
      let eitherErrOk = runTranslation custEnv m
      toMonad eitherErrOk


-------------------------------------------------------------------------------
-- - wrapping -
-------------------------------------------------------------------------------


-- | Does a commit iff the action is executed successfully.
inTransaction :: Monad a -> Monad a
inTransaction action = do
  res  <- action
  conn <- getEnv envDbConn
  liftIO $ HDBC.commit conn
  pure res


-------------------------------------------------------------------------------
-- - SQL statement preparation -
-------------------------------------------------------------------------------


prepareSql :: SQL_IDENTIFIER col
           => SqlDmlStatement col
           -> Monad HDBC.Statement
prepareSql sql = do
  (conn, sqlRenderer) <- getEnvConnAndSqlRenderer
  let sqlString = sqlRenderer $ fmap sqlIdentifier sql
  Logging.logg $ "prepareSql:\n" ++ sqlString
  liftIO $ HDBC.prepare conn sqlString

prepareSql_str :: String -- ^ SQL statement
               -> Monad HDBC.Statement
prepareSql_str sql = do
  Logging.logg $"prepareSql_str:\n" ++ sql
  conn <- getEnvConn
  liftIO $ HDBC.prepare conn sql

getEnvConn :: Monad HDBC.ConnWrapper
getEnvConn = getEnv envDbConn

getEnvConnAndSqlRenderer :: Monad (HDBC.ConnWrapper, DmlRenderer)
getEnvConnAndSqlRenderer = getEnv getConnAndSqlRend
  where
    getConnAndSqlRend env = (envDbConn env, envSqlRenderer env)
