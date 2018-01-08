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

-------------------------------------------------------------------------------
-- | Execution of SQL DELETE statements.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Database.Execution.Delete
       (
         delete,
         deleteAll,
         deleteOne,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC

import qualified Wilde.Database.Sql as Sql

import qualified Wilde.Database.Executor as SqlExec

import Wilde.ObjectModel.ObjectModel

import Wilde.Media.Database.Monad

import qualified Wilde.ObjectModel.Database.Sql.SansPresentationInfo as SqlPlain
import qualified Wilde.ObjectModel.Database.Output as Output

import qualified Wilde.ObjectModel.Database as Database

import qualified Wilde.ObjectModel.Database.Execution.Utils as ExecUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Deletes all objects/rows.
--
-- Returns the number of deleted rows.  'Nothing' means this info is not
-- provided by the database backen.
-------------------------------------------------------------------------------
deleteAll :: (Database.DATABASE_TABLE otConf
             ,Database.COLUMN_NAMES atConf
             )
          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> SqlExec.ConnectionAndRenderer
          -> DatabaseMonad (Maybe Integer)
deleteAll ot@(ObjectType {}) car =
  ExecUtils.execForWhereExpr (newDeleteDml ot) Nothing [] car

-------------------------------------------------------------------------------
-- | Deletes a selection of objects/rows.
--
-- Returns the number of deleted rows.  'Nothing' means this info is not
-- provided by the database backen.
-------------------------------------------------------------------------------
delete :: (Database.DATABASE_TABLE otConf
          ,Database.COLUMN_NAMES atConf
          )
       => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
       -> Maybe (Sql.SqlExpr dbTable)
       -- ^ WHERE expression.
       -> [SqlValue]
       -- ^ Parameters of the given WHERE expression.
       -> SqlExec.ConnectionAndRenderer
       -> DatabaseMonad (Maybe Integer)
delete ot@(ObjectType {}) mbWhereExpr sqlParameters car =
  ExecUtils.execForWhereExpr (newDeleteDml ot) mbWhereExpr sqlParameters car

-------------------------------------------------------------------------------
-- | Deletes the object/rows that is identified by the given
-- ID-attribute value.
--
-- Returns the number of deleted rows.  'Nothing' means this info is not
-- provided by the database backen.
-------------------------------------------------------------------------------
deleteOne :: (Output.DATABASE_TABLE otConf
             ,Output.OUTPUT_FOR_EXISTING atConf
             ,Database.COLUMN_NAMES atConf
             )
          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> idAtExisting
          -> SqlExec.ConnectionAndRenderer
          -> DatabaseMonad (Maybe Integer)
deleteOne ot@(ObjectType {}) idAtValue car =
  ExecUtils.execForIdAtObject newDeleteDml ot idAtValue ([],[]) car

-------------------------------------------------------------------------------
-- | Utility that constructs SQL DML statement for delete.
-------------------------------------------------------------------------------
newDeleteDml :: (Database.DATABASE_TABLE otConf
                ,Database.COLUMN_NAMES atConf
                )
             => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
             -> Maybe (Sql.SqlExpr dbTable) 
             -> Sql.SqlDmlStatement dbTable    
newDeleteDml ot = Sql.SqlDmlDelete . SqlPlain.delete ot
