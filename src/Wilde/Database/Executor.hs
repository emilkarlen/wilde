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

-- | Information about a database driver.
module Wilde.Database.Executor
       (
         -- * Configuration

         Configuration(..),
         DmlExecutor,
         DmlRenderer,
         toRenderable,

         ConnectionAndRenderer(..),

         MonadWithDatabaseConfiguration(..),
         MonadIO(..),

         getDatabaseConnectionAndRenderer,
         getDatabaseConnection,

         -- * Utilities for executing a DML statement


         quick,
         quickSelect,
         quickInsert,
         quickUpdate,
         quickDelete,
         
         quickNumRows,
         quickNumRows2
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.IO.Class

import Database.HDBC

import Wilde.Database.Sql


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Configuation of the database driver for an application.
data Configuration =
  Configuration
  {
    connectionProvider :: IO ConnWrapper
  , dmlRenderer        :: DmlRenderer
  }

data ConnectionAndRenderer =
  ConnectionAndRenderer
  {
    carConnection :: ConnWrapper
  , carRenderer   :: SqlDmlStatement SqlIdentifier -> String
  }

-- | Database driver function that executes a DML statement.
type DmlExecutor = SqlDmlStatement SqlIdentifier -> ConnWrapper -> [SqlValue] -> IO [[SqlValue]]

-- | Rendering of DML SQL for a database driver.
type DmlRenderer = SqlDmlStatement SqlIdentifier -> String

-- | Prepares a structure for rendering.
toRenderable :: (Functor t,SQL_IDENTIFIER a)
             => t a
             -> t SqlIdentifier
toRenderable = fmap sqlIdentifier


-------------------------------------------------------------------------------
-- - monad -
-------------------------------------------------------------------------------


class MonadIO m => MonadWithDatabaseConfiguration m where
  getDatabaseConfiguration :: m Configuration

getDatabaseConnection :: MonadWithDatabaseConfiguration m
                      => m ConnWrapper
getDatabaseConnection =
  do
    Configuration connProvider dmlRenderer <- getDatabaseConfiguration
    liftIO connProvider

getDatabaseConnectionAndRenderer :: MonadWithDatabaseConfiguration m
                                 => m ConnectionAndRenderer
getDatabaseConnectionAndRenderer =
  do
    Configuration connProvider dmlRenderer <- getDatabaseConfiguration
    conn <- liftIO connProvider
    return $
      ConnectionAndRenderer
      {
        carConnection = conn
      , carRenderer   = dmlRenderer
      }

quick :: (MonadIO m,SQL_IDENTIFIER col)
      => ConnectionAndRenderer
      -> SqlDmlStatement col
      -> [SqlValue]
      -> m [[SqlValue]]
quick (ConnectionAndRenderer conn renderer) sql params =
  liftIO $ quickQuery conn sqlString params
  where
    sqlString = renderer $ fmap sqlIdentifier sql

quickSelect :: (MonadIO m,SQL_IDENTIFIER col)
            => ConnectionAndRenderer
            -> SqlSelect col
            -> [SqlValue]
            -> m [[SqlValue]]
quickSelect (ConnectionAndRenderer conn renderer) sql params =
  liftIO $ quickQuery conn sqlString params
  where
    sqlString = renderer $ fmap sqlIdentifier (SqlDmlSelect sql)

quickInsert :: (MonadIO m,SQL_IDENTIFIER col)
            => ConnectionAndRenderer
            -> SqlInsert col
            -> [SqlValue]
            -> m Integer
quickInsert car sql = quickNumRows car (SqlDmlInsert sql)

quickUpdate :: (MonadIO m,SQL_IDENTIFIER col)
            => ConnectionAndRenderer
            -> SqlUpdate col
            -> [SqlValue]
            -> m Integer
quickUpdate car sql = quickNumRows car (SqlDmlUpdate sql)

quickDelete :: (MonadIO m,SQL_IDENTIFIER col)
            => ConnectionAndRenderer
            -> SqlDelete col
            -> [SqlValue]
            -> m Integer
quickDelete car sql = quickNumRows car (SqlDmlDelete sql)

quickNumRows :: (MonadIO m,SQL_IDENTIFIER col)
             => ConnectionAndRenderer
             -> SqlDmlStatement col
             -> [SqlValue]
             -> m Integer
quickNumRows (ConnectionAndRenderer conn renderer) sql params =
  liftIO $ do
    stmt <- prepare conn sqlString
    execute stmt params
  where
    sqlString = renderer $ fmap sqlIdentifier sql

-------------------------------------------------------------------------------
-- | A variant of 'quickNumRows' that returns 'Nothing' if the database
-- backend does not supply the number of matching rows.
-------------------------------------------------------------------------------
quickNumRows2 :: (MonadIO m,SQL_IDENTIFIER col)
              => ConnectionAndRenderer
              -> SqlDmlStatement col
              -> [SqlValue]
              -> m (Maybe Integer)
quickNumRows2 (ConnectionAndRenderer conn renderer) sql params =
  liftIO $ do
    stmt <- prepare conn sqlString
    numRows <- execute stmt params
    return $ if numRows < 0
             then Nothing
             else Just numRows
  where
    sqlString = renderer $ fmap sqlIdentifier sql


-------------------------------------------------------------------------------
-- - Utilities for executing DML -
-------------------------------------------------------------------------------


-- -- | Executes a DML statement for any column type.
-- executeAny :: SQL_IDENTIFIER col
--            => DmlExecutor
--            -> SqlDmlStatement col
--            -> ConnWrapper
--            -> [SqlValue]
--            -> IO [[SqlValue]]
-- executeAny executor stmt conn params = executor stmt' conn params
--   where stmt' = fmap sqlIdentifier stmt

-- -- | Executes a DML statement for any column type.
-- executeAnyForConf :: SQL_IDENTIFIER col
--                   => Configuration
--                   -> SqlDmlStatement col
--                   -> [SqlValue]
--                   -> IO [[SqlValue]]
-- executeAnyForConf (Configuration theConnectionProvider theExecutor) stmt params =
--   do
--     conn <- theConnectionProvider
--     theExecutor stmt' conn params
--   where stmt' = fmap sqlIdentifier stmt

-- -- | Executes a DML statement for any column type.
-- executeAnyForConfWithConn :: SQL_IDENTIFIER col
--                           => Configuration
--                           -> SqlDmlStatement col
--                           -> ConnWrapper
--                           -> [SqlValue]
--                           -> IO [[SqlValue]]
-- executeAnyForConfWithConn conf = executeAny (dmlExecutor conf)
