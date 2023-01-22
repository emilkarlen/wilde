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

import qualified Wilde.Media.Database.Monad as DbConn

import Wilde.ObjectModel.ObjectModel


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
          -> DbConn.Monad (Maybe Integer)
deleteAll ot@(ObjectType {}) =
  ExecUtils.execForWhereExpr (newDeleteDml ot) Nothing []

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
       -> DbConn.Monad (Maybe Integer)
delete ot@(ObjectType {}) mbWhereExpr sqlParameters =
  ExecUtils.execForWhereExpr (newDeleteDml ot) mbWhereExpr sqlParameters

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
          -> DbConn.Monad (Maybe Integer)
deleteOne ot@(ObjectType {}) idAtValue =
  ExecUtils.execForIdAtObject newDeleteDml ot idAtValue ([],[])

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
