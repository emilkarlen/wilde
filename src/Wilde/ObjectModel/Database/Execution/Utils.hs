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
-- | Utilities related to execution of SQL statements in db monad.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Database.Execution.Utils
       (
         execForWhereExpr,
         execForOne,
         execForIdAtObject,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC

import qualified Wilde.Database.Sql as Sql

import Wilde.ObjectModel.ObjectModel

import qualified Wilde.Media.Database.Monad as DbConnM
import qualified Wilde.Media.Database.Exec as Exec

import qualified Wilde.ObjectModel.Database.Output as Output

import qualified Wilde.ObjectModel.Database.Utils as Utils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data DmlExecutionInfoConstruction dbTable =
  DmlExecutionInfoConstruction
  {
    newDml :: Maybe (Sql.SqlExpr dbTable) -> Sql.SqlDmlStatement dbTable
  , sqlParamsBeforeWhereExpr :: [SqlValue]
  , sqlParamsAfterWhereExpr  :: [SqlValue]
  }

-------------------------------------------------------------------------------
-- | Executes a SQL statement where the WHERE expression is equality on
-- the ID 'Attribute' of an 'ObjectType'.
--
-- Returns the number of rows affected by the statement.
-------------------------------------------------------------------------------
execForIdAtObject :: (Output.OUTPUT_FOR_EXISTING atConf
                     ,Output.COLUMN_NAMES atConf
                     )
                  => (ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                      -> Maybe (Sql.SqlExpr dbTable) 
                      -> Sql.SqlDmlStatement dbTable)
                  -> ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                  -> idAtExisting
                  -> ([SqlValue],[SqlValue])
                  -- ^ Parameters of the resulting SQL statement that
                  -- (precedes,comes after), respectively,
                  -- those of the WHERE expression.
                  -> DbConnM.Monad (Maybe Integer)
execForIdAtObject newSqlForWhereExpr
  ot@(ObjectType {})
  idAtValue
  sqlParams 
  =
    execForOne (newSqlForWhereExpr ot) idAt idAtValue sqlParams
  where
    idAt = otIdAttributeType ot

-------------------------------------------------------------------------------
-- | Executes a SQL statement where the WHERE expression is equality on
-- a single 'Attribute' given by it's type and value.
--
-- Returns the number of rows affected by the statement.
-------------------------------------------------------------------------------
execForOne :: (Output.OUTPUT_FOR_EXISTING atConf
              ,Output.COLUMN_NAMES atConf
              )
           => (Maybe (Sql.SqlExpr dbTable) -> Sql.SqlDmlStatement dbTable)
           -> AttributeType atConf dbTable typeForExisting typeForCreate
           -- ^ The single AttributeType for which the WHERE is constructed
           -- by an equality expression.
           -> typeForExisting
           -> ([SqlValue],[SqlValue])
           -- ^ Parameters of the resulting SQL statement that
           -- (precedes,comes after), respectively,
           -- those of the WHERE expression.
           -> DbConnM.Monad (Maybe Integer)
execForOne newSqlForWhereExpr 
  whereEqAttrType@(AttributeType {})
  whereEqAttrValue 
  (sqlParamsBeforeWhereExpr,sqlParamsAfterWhereExpr) 
  =
  do
    whereEqAttrSqlValues <- DbConnM.toMonad getAtSqlValues
    let sqlParams         = sqlParamsBeforeWhereExpr ++ 
                            whereEqAttrSqlValues ++
                            sqlParamsAfterWhereExpr
    execForWhereExpr newSqlForWhereExpr mbWhereExpr sqlParams
  where
    getAtSqlValues   = Output.atOutputerExisting whereEqAttrType whereEqAttrValue
                       :: ConvertResult [SqlValue]
    mbWhereExpr      = Utils.justAtEqPosParamExpr whereEqAttrType
    
-------------------------------------------------------------------------------
-- | Executes a SQL statement given a way to construct it from a given WHERE
-- expression, and the SQL parameters expected by the constructed SQL.
--
-- Returns the number of rows affected by the statement.
-------------------------------------------------------------------------------
execForWhereExpr :: Sql.SQL_IDENTIFIER dbTable
                 => (Maybe (Sql.SqlExpr dbTable) -> Sql.SqlDmlStatement dbTable)
                 -> Maybe (Sql.SqlExpr dbTable)
                 -> [SqlValue]
                 -> DbConnM.Monad (Maybe Integer)
execForWhereExpr newSqlForWhereExpr mbWhereExpr sqlParameters =
  Exec.execSql_numRowsMb (newSqlForWhereExpr mbWhereExpr) sqlParameters
