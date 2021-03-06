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
-- | Selection of plain objects - objects without presentation information
-- that is derived from anything else than the representation value
-- of the 'AttributeType'.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Database.Execution.SelectSansPresentationInfo
       (
         
         selectAll,
         
         selectOne,
         selectOneWithOutputer,
         selectOneWithSqlValues,
         
         selectSelection,
         select,
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

import qualified Wilde.ObjectModel.Database.InputExistingSansPresentationInfo as InputExisting
import qualified Wilde.ObjectModel.Database.Output as Output
import qualified Wilde.ObjectModel.Database.Sql.SansPresentationInfo as SqlPlain
import           Wilde.ObjectModel.Database as Database


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Reads all objects (plain) of a given 'ObjectType' from the database.
-------------------------------------------------------------------------------
selectAll :: (Database.DATABASE_TABLE otConf
             ,Database.COLUMN_NAMES atConf
             ,InputExisting.INPUT_FOR_EXISTING atConf
             )
          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> [Any (AttributeType atConf dbTable)]
          -- ^ ORDER BY
          -> SqlExec.ConnectionAndRenderer
          -> DatabaseMonad [Object otConf atConf dbTable otNative idAtExisting idAtCreate]
selectAll ot@(ObjectType {}) orderBy car =
  select ot sql [] car
  where
    sql = SqlPlain.selectAll ot orderBy

-------------------------------------------------------------------------------
-- | Selects a single object given the value of the ID 'AttributeType'.
-------------------------------------------------------------------------------
selectOne :: (Database.DATABASE_TABLE otConf
             ,Database.COLUMN_NAMES atConf
             ,InputExisting.INPUT_FOR_EXISTING atConf
             ,Output.OUTPUT_FOR_EXISTING atConf
             )
          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> idAtExisting
          -> SqlExec.ConnectionAndRenderer
          -> DatabaseMonad (Maybe (Object otConf atConf dbTable otNative idAtExisting idAtCreate))
selectOne ot@(ObjectType {}) idAtValue car =
  selectOneWithOutputer
  ot 
  (Output.atOutputerExisting (otIdAttributeType ot))
  idAtValue
  car 

-------------------------------------------------------------------------------
-- | A variant of 'selectOne' that takes a 'DatabaseOutputer' for the
-- ID 'AttributeType' as argument.
-------------------------------------------------------------------------------
selectOneWithOutputer :: (Database.DATABASE_TABLE otConf
                         ,Database.COLUMN_NAMES atConf
                         ,InputExisting.INPUT_FOR_EXISTING atConf
                         )
                      => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                      -> DatabaseOutputer idAtExisting
                      -> idAtExisting
                      -> SqlExec.ConnectionAndRenderer
                      -> DatabaseMonad (Maybe (Object otConf atConf dbTable otNative idAtExisting idAtCreate))
selectOneWithOutputer ot@(ObjectType {}) idAtValueOutputer idAtValue car =
  do
    idAtSqlValues <- toDatabaseMonad $
                     idAtValueOutputer idAtValue
    selectOneWithSqlValues ot idAtSqlValues car

-------------------------------------------------------------------------------
-- | A variant of 'selectOne' that takes the 'SqlValue's for the
-- ID 'AttributeType' as argument.
-------------------------------------------------------------------------------
selectOneWithSqlValues :: (Database.COLUMN_NAMES atConf
                          ,InputExisting.INPUT_FOR_EXISTING atConf
                          ,Database.DATABASE_TABLE otConf)
                       => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                       -> [SqlValue]
                       -> SqlExec.ConnectionAndRenderer
                       -> DatabaseMonad (Maybe (Object otConf atConf dbTable otNative idAtExisting idAtCreate))
selectOneWithSqlValues ot@(ObjectType {}) idAtSqlValues car =
  do
    databaseInput <- SqlExec.quickSelect car sql idAtSqlValues
    case databaseInput of
      []    -> return Nothing
      [row] -> do o <- toDatabaseMonad $ InputExisting.inputObject ot row
                  return $ Just o
      _     -> let msg = "Expected none or a single row. Got " ++ show databaseInput
               in  throwErr $ ImplementationTranslationError msg
  where
    sql = SqlPlain.selectOne ot
    
-------------------------------------------------------------------------------
-- | Custom selection of objects. 
-------------------------------------------------------------------------------
selectSelection :: (Database.COLUMN_NAMES atConf
                   ,Database.DATABASE_TABLE otConf
                   ,InputExisting.INPUT_FOR_EXISTING atConf)
                => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                -> Maybe (Sql.SqlExpr dbTable)
                -- ^ SELECT that selects exactly the column that correspond
                -- to the 'AttributeType's of the 'ObjectType'.
                -> [Any (AttributeType atConf dbTable)]
                -- ^ ORDER BY columns
                -> [SqlValue]
                -- ^ Parameters of the SELECT statement.
                -> SqlExec.ConnectionAndRenderer
                -> DatabaseMonad [Object otConf atConf dbTable otNative idAtExisting idAtCreate]
selectSelection ot mbWhereExpr orderBy sqlParamsInWhereExpr car =
  select ot sql sqlParamsInWhereExpr car
  where
    sql = SqlPlain.select ot mbWhereExpr orderBy

-------------------------------------------------------------------------------
-- | Reads 'Object's (plain) of a given 'ObjectType' from the database,
-- that is the result of a given SELECT statement.
-------------------------------------------------------------------------------
select :: (Database.COLUMN_NAMES atConf
          ,InputExisting.INPUT_FOR_EXISTING atConf)
       => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
       -> Sql.SqlSelect dbTable
       -- ^ SELECT that selects exactly the column that correspond
       -- to the 'AttributeType's of the 'ObjectType'.
       -> [SqlValue]
       -- ^ Parameters of the SELECT statement.
       -> SqlExec.ConnectionAndRenderer
       -> DatabaseMonad [Object otConf atConf dbTable otNative idAtExisting idAtCreate]
select ot@(ObjectType {}) sql sqlParamsInWhereExpr car =
  do
    sqlValueListList <- SqlExec.quickSelect car sql sqlParamsInWhereExpr
    toDatabaseMonad $ mapM (InputExisting.inputObject ot) sqlValueListList
