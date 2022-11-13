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
