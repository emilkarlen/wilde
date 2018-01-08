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
-- | Generation of SQL statements that select
-- the \"plain\" variant of objects.
--
-- \"Plain\" means that only information that
-- constitutes each 'AttributeType's \"representation\"
-- is selected - not information\ that is only used
-- for \"presentation\".
--
-- This means that only columns from the
-- 'ObjectType's own table are selected.  Also, only
-- information 
-------------------------------------------------------------------------------
module Wilde.ObjectModel.Database.Sql.SansPresentationInfo
       (
         -- * INSERT
         
         insertOne,
         
         -- * SELECT
         
         select,
         selectAll,
         selectOne,
         
         -- * UPDATE
         
         update_attributes,
         
         updateOne,
         updateOne_attributes,
         
         -- * DELETE
         
         delete,
         deleteAll,
         deleteOne,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.ObjectModel.ObjectModelUtils

import qualified Wilde.Database.Sql as Sql
import qualified Wilde.Database.SqlMisc as SqlMisc

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Media.Database

import Wilde.ObjectModel.Database.Utils
import Wilde.ObjectModel.Database


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | SQL for inserting a single object into the database.
--
-- The SQL contains a positional parameter for each column.
-- The columns are listed in the order they appear in the 'ObjectType':
-- first the ID-attribute, then the other attributes in the order they
-- appear in the list.
-------------------------------------------------------------------------------
insertOne :: (COLUMN_NAMES atConf
             ,DATABASE_TABLE otConf)
          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> Sql.SqlInsert dbTable
insertOne ot = Sql.insert tblName colNames (map (const Sql.posParam) colNames)
  where
    tblName        = tableName $ otDatabaseTable ot
    allDbCols      = concatMap atColumnListAny$ otAttributeTypes ot
    colNames       = map columnName allDbCols
   

{- |
An SQL SELECT statement that selects all columns and all rows.

Gives a SELECT statement where the select expressions ...

 * each expression is a table column

 * the columns for the 'AttributeType's are listed in the
   same order as the 'AttributeType's are listed in the 'ObjectType'.

 * for each 'AttributeType', it's columns are listed in the same
   order as they are listed in that object's database structure.
-}
selectAll :: (COLUMN_NAMES atConf
             ,DATABASE_TABLE otConf)
          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> [Any (AttributeType atConf dbTable)]
          -> Sql.SqlSelect dbTable
selectAll ot orderBy = SqlMisc.selectRep tblName colNames Nothing (getOrderByCols orderBy)
  where
    tblName  = tableName $ otDatabaseTable ot
    (ObjectTranslationSetup
     {
       setupIdAtColumns     = dbColsIdAt
     , setupNonIdAtsColumns = dbColsNonIdAts
     }
      )      = inputPlainObjectSetup ot
    allCols  = dbColsIdAt : dbColsNonIdAts
    colNames = map columnName $ concat allCols

{- |
Utility method for constructing SELECT statements that
select the columns that correspond to the attribute-types of
the 'ObjectType'.

Gives a SQL SELECT statement that selects all columns and some rows.

Parameters to the SQL are only those of the WHERE expression.

Gives a SELECT statement where the select expressions ...

 * each expression is a table column

 * the columns for the 'AttributeType's are listed in the
   same order as the 'AttributeType's are listed in the 'ObjectType'.

 * for each 'AttributeType', it's columns are listed in the same
   order as they are listed in that object's database structure.
-}
select :: (COLUMN_NAMES atConf
          ,DATABASE_TABLE otConf)
       => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
       -> Maybe (Sql.SqlExpr dbTable)              -- ^ WHERE expression
       -> [Any (AttributeType atConf dbTable)] -- ^ ORDER BY columns
       -> Sql.SqlSelect dbTable
select ot mbWhereExpr orderBy =
  SqlMisc.selectRep tblName colNames mbWhereExpr (getOrderByCols orderBy)
  where
    tblName  = tableName $ otDatabaseTable ot
    (ObjectTranslationSetup
     {
       setupIdAtColumns     = idAtColumns
     , setupNonIdAtsColumns = nonIdAtsColumns
     }
      )      = inputPlainObjectSetup ot
    allCols  = idAtColumns : nonIdAtsColumns
    colNames = map columnName $ concat allCols

-------------------------------------------------------------------------------
-- | SQL for selecting a single 'Object'.
--
-- Parameters of the statement are the ID-attribute of type idAtExisting.
-------------------------------------------------------------------------------
selectOne :: (COLUMN_NAMES atConf 
             ,DATABASE_TABLE otConf)
          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> Sql.SqlSelect dbTable
selectOne ot = select ot (justOtIdAtEqPosParamExpr ot) []
    
-------------------------------------------------------------------------------
-- | An UPDATE statement which updates the given list of 'AttributeType's,
-- of the selected objects/rows.
-------------------------------------------------------------------------------
update_attributes :: (COLUMN_NAMES atConf
                     ,DATABASE_TABLE otConf)
                  => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                  -> NonEmpty.List (Any (AttributeType atConf dbTable))
                  -- ^ Attributes to update
                  -> Maybe (Sql.SqlExpr dbTable) 
                  -- ^ WHERE expr
                  -> Sql.SqlUpdate dbTable
update_attributes ot atsToUpdate mbWhereExpr = 
  Sql.update theTableName colSets mbWhereExpr
  where
    colSets       = fmap setColumnName colsToUpdate
    colsToUpdate  = NonEmpty.concat $ fmap (anyValueApply atColumnNames) atsToUpdate
    theTableName  = tableName $ otDatabaseTable ot
    -- Transforms a column name (dbTable) to one of the items in the
    -- UPDATE clauses SET list of (col = expr).
    setColumnName :: dbTable -> (dbTable,Sql.SqlExpr dbTable)
    setColumnName colName = (colName,Sql.posParam)

-------------------------------------------------------------------------------
-- | SQL for updating an 'Object'.
--
-- Otherwise, a 'SqlUpdate' is returned.  This SQL has positional parameters
-- for
--
-- * the columns of each 'AttributeType' that is updatable, in the order they
-- appear in the 'ObjectType', followed by
--
-- * the columns of the ID 'AttributeType'.
-------------------------------------------------------------------------------
updateOne :: (COLUMN_NAMES atConf
             ,DATABASE_TABLE otConf)
          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> NonEmpty.List (Any (AttributeType atConf dbTable))
          -> Sql.SqlUpdate dbTable
updateOne ot atsToUpdate =
  update_attributes ot atsToUpdate (justOtIdAtEqPosParamExpr ot)

-------------------------------------------------------------------------------
-- | A variant of 'updateOne' where the 'AttributeType's to update
-- are given explicitly.
-------------------------------------------------------------------------------
updateOne_attributes :: (COLUMN_NAMES atConf
                        ,DATABASE_TABLE otConf
                        )
                     => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                     -> NonEmpty.List (Any (AttributeType atConf dbTable))
                     -- ^ Attributes to update.
                     -- The SQL expects as parameters values for all these
                     -- attributes.
                     -> Sql.SqlUpdate dbTable
updateOne_attributes ot atsToUpdate = 
  update_attributes ot atsToUpdate (justOtIdAtEqPosParamExpr ot)

-------------------------------------------------------------------------------
-- | SQL for deleting an 'Object'.
--
-- A  'SqlDelete' is returned.  This SQL has positional parameters
-- for the columns of the ID 'AttributeType'.
-------------------------------------------------------------------------------
delete :: (COLUMN_NAMES atConf
          ,DATABASE_TABLE otConf)
       => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
       -> Maybe (Sql.SqlExpr dbTable) -- ^ WHERE expression
       -> Sql.SqlDelete dbTable
delete ot mbWhereExpr = Sql.delete theTableName mbWhereExpr
  where
    theTableName  = tableName $ otDatabaseTable ot

-------------------------------------------------------------------------------
-- | SQL for deleting all records of a table.
--
-- A  'SqlDelete' is returned.  This SQL has NO parameters.
-------------------------------------------------------------------------------
deleteAll :: (COLUMN_NAMES atConf
             ,DATABASE_TABLE otConf)
          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> Sql.SqlDelete dbTable
deleteAll ot = delete ot Nothing

-------------------------------------------------------------------------------
-- | SQL for deleting an 'Object'.
--
-- A  'SqlDelete' is returned.  This SQL has positional parameters
-- for the columns of the ID 'AttributeType'.
-------------------------------------------------------------------------------
deleteOne :: (COLUMN_NAMES atConf
             ,DATABASE_TABLE otConf)
          => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
          -> Sql.SqlDelete dbTable
deleteOne ot = delete ot (justOtIdAtEqPosParamExpr ot)
