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
-- | Execution of SQL UPDATE statements.
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Database.Execution.Update
       (
         -- * All objects
         
         updateAll_attributes,
         
         -- * One object
         
         updateOne,
         updateOne_attributes,
         
         -- * Selection of objects
         
         update_attributes,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import qualified Wilde.Database.Sql as Sql

import qualified Wilde.Database.Executor as SqlExec

import Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.ObjectModelUtils as OmUtils

import Wilde.Media.Database.Monad

import qualified Wilde.ObjectModel.Database.Sql.SansPresentationInfo as SqlPlain
import qualified Wilde.ObjectModel.Database.Output as Output

import qualified Wilde.ObjectModel.Database.Utils as Utils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Updates the given 'Attribute's of all objects.
--
-- Returns the number of updated rows.  'Nothing' means this info is not provided
-- by the database backen.
-------------------------------------------------------------------------------
updateAll_attributes :: (Output.DATABASE_TABLE otConf
                        ,Output.OUTPUT_FOR_EXISTING atConf
                        ,Output.COLUMN_NAMES atConf
                        )
                     => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                     -> NonEmpty.List (Any (Attribute atConf dbTable))
                     -> SqlExec.ConnectionAndRenderer
                     -> DatabaseMonad (Maybe Integer)
updateAll_attributes ot attrsToUpdate car =
  update_attributes ot Nothing [] attrsToUpdate car

-------------------------------------------------------------------------------
-- | Updates the object that is identified by the given ID-attribute value.
--
-- The attributes that are updated are the standard updatable attributes.
-------------------------------------------------------------------------------
updateOne :: (Output.DATABASE_TABLE otConf
             ,Output.OUTPUT_FOR_EXISTING atConf
             ,Output.COLUMN_NAMES atConf
             )
          => Object otConf atConf dbTable otNative idAtExisting idAtCreate
          -> NonEmpty.List (Any (Attribute atConf dbTable))
          -> SqlExec.ConnectionAndRenderer
          -> DatabaseMonad (Maybe Integer)
updateOne o attrsToUpdate car =
  updateOne_attributes
  (oType o) 
  idAtValue 
  attrsToUpdate
  car
  where
    idAtValue = attrValue $ oIdAttribute o

-------------------------------------------------------------------------------
-- | Updates the given 'Attribute's of the given 'Object'.
-------------------------------------------------------------------------------
updateOne_attributes :: (Output.DATABASE_TABLE otConf
                        ,Output.COLUMN_NAMES atConf
                        ,Output.OUTPUT_FOR_EXISTING atConf
                        )
                     => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                     -> idAtExisting
                     -> NonEmpty.List (Any (Attribute atConf dbTable))
                     -> SqlExec.ConnectionAndRenderer
                     -> DatabaseMonad (Maybe Integer)
updateOne_attributes ot@(ObjectType {}) idAtValue attrsToUpdate car =
  do
    idAtSqlValues <- toDatabaseMonad getIdAtSqlValues
    update_attributes
      ot 
      (Utils.justOtIdAtEqPosParamExpr ot)
      idAtSqlValues 
      attrsToUpdate 
      car
  where
    getIdAtSqlValues = Output.atOutputerExisting (otIdAttributeType ot) idAtValue
                       :: ConvertResult [SqlValue]

-------------------------------------------------------------------------------
-- | Updates the given 'Attribute's of objects that
-- matches a given WHERE expression.
--
-- Returns the number of updated rows. 'Nothing' means that this info is not
-- returned by the DB backed.
-------------------------------------------------------------------------------
update_attributes :: (Output.DATABASE_TABLE otConf
                     ,Output.COLUMN_NAMES atConf
                     ,Output.OUTPUT_FOR_EXISTING atConf
                     )
                  => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                  -> Maybe (Sql.SqlExpr dbTable) 
                  -- ^ WHERE expression
                  -> [SqlValue]
                  -- ^ SQL parameters for the WHERE expression
                  -> NonEmpty.List (Any (Attribute atConf dbTable))
                  -- ^ ORDER BY
                  -> SqlExec.ConnectionAndRenderer
                  -> DatabaseMonad (Maybe Integer)
update_attributes ot@(ObjectType {}) mbWhereExpr whereExprParams attrsToUpdate car =
   do
     paramValuesForUpdates <- toDatabaseMonad getAttrsToUpdateSqlValues
     let paramValues = paramValuesForUpdates ++ whereExprParams
     numUpdatedRows <- SqlExec.quickUpdate car updateStmt paramValues
     return $ if numUpdatedRows < 0
              then Nothing
              else Just numUpdatedRows
  where
    atsToUpdate    = getAts attrsToUpdate
    updateStmt     = SqlPlain.update_attributes ot atsToUpdate mbWhereExpr
    getAttrsToUpdateSqlValues = fmap
                                concat
                                (Output.aOutputsExisting (NonEmpty.toList attrsToUpdate))
                                :: ConvertResult [SqlValue]
                                   
    getAts :: NonEmpty.List (Any (Attribute     atConf dbTable))
           -> NonEmpty.List (Any (AttributeType atConf dbTable))
    getAts attrs = fmap (OmUtils.anyValueApply2 attrType) attrs


-------------------------------------------------------------------------------
-- | Executes a SQL statement where the WHERE expression is quality on
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
           -> [SqlValue]
           -- ^ Parameters of the resulting SQL statement that precedes
           -- those of the WHERE expression.
           -> SqlExec.ConnectionAndRenderer
           -> DatabaseMonad (Maybe Integer)
execForOne newSqlForWhereExpr 
  whereEqAttrType@(AttributeType {})
  whereEqAttrValue 
  sqlParamsBeforeWhereExpr 
  car =
  do
    whereEqAttrSqlValues <- toDatabaseMonad getAtSqlValues
    let sqlParams         = sqlParamsBeforeWhereExpr ++ whereEqAttrSqlValues
    execForWhereExpr newSqlForWhereExpr mbWhereExpr sqlParams car
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
                 -> SqlExec.ConnectionAndRenderer
                 -> DatabaseMonad (Maybe Integer)
execForWhereExpr newSqlForWhereExpr mbWhereExpr sqlParameters car =
  SqlExec.quickNumRows2 car (newSqlForWhereExpr mbWhereExpr) sqlParameters
