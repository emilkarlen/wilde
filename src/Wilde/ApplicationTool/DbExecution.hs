-- | Information about a database driver.
module Wilde.ApplicationTool.DbExecution
       (
         -- * Configuration

         DmlExecutor,
         DmlRenderer,
         toRenderable,

         ConnectionAndRenderer(..),

         MonadIO(..),

         -- * Utilities for executing a DML statement

         quickSelect,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.IO.Class

import Database.HDBC

import Wilde.Database.Sql
import Wilde.Database.DmlRenderer

-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data ConnectionAndRenderer =
  ConnectionAndRenderer
  {
    carConnection :: ConnWrapper
  , carRenderer   :: DmlRenderer
  }

-- | Database driver function that executes a DML statement.
type DmlExecutor = SqlDmlStatement SqlIdentifier -> ConnWrapper -> [SqlValue] -> IO [[SqlValue]]


-------------------------------------------------------------------------------
-- - monad -
-------------------------------------------------------------------------------


quickSelect :: (MonadIO m,SQL_IDENTIFIER col)
            => ConnectionAndRenderer
            -> SqlSelect col
            -> [SqlValue]
            -> m [[SqlValue]]
quickSelect (ConnectionAndRenderer conn renderer) sql params =
  liftIO $ quickQuery conn sqlString params
  where
    sqlString = renderer $ fmap sqlIdentifier (SqlDmlSelect sql)
