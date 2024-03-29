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

import qualified Data.List.NonEmpty as NonEmpty

import qualified Wilde.Database.Sql as Sql

import qualified Wilde.Media.Database.Exec as SqlExec

import Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.ObjectModelUtils as OmUtils

import qualified Wilde.Media.Database.Monad as DbConn

import qualified Wilde.ObjectModel.Database.Sql.SansPresentationInfo as SqlPlain
import qualified Wilde.ObjectModel.Database.Output as Output

import qualified Wilde.ObjectModel.Database.Utils as Utils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Updates the given 'Attribute's of all objects.
--
-- pures the number of updated rows.  'Nothing' means this info is not provided
-- by the database backen.
-------------------------------------------------------------------------------
updateAll_attributes :: (Output.DATABASE_TABLE otConf
                        ,Output.OUTPUT_FOR_EXISTING atConf
                        ,Output.COLUMN_NAMES atConf
                        )
                     => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                     -> NonEmpty.NonEmpty (Any (Attribute atConf dbTable))
                     -> DbConn.Monad (Maybe Integer)
updateAll_attributes ot attrsToUpdate =
  update_attributes ot Nothing [] attrsToUpdate

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
          -> NonEmpty.NonEmpty (Any (Attribute atConf dbTable))
          -> DbConn.Monad (Maybe Integer)
updateOne o attrsToUpdate =
  updateOne_attributes
  (oType o)
  idAtValue
  attrsToUpdate
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
                     -> NonEmpty.NonEmpty (Any (Attribute atConf dbTable))
                     -> DbConn.Monad (Maybe Integer)
updateOne_attributes ot@(ObjectType {}) idAtValue attrsToUpdate =
  do
    idAtSqlValues <- DbConn.toMonad getIdAtSqlValues
    update_attributes
      ot
      (Utils.justOtIdAtEqPosParamExpr ot)
      idAtSqlValues
      attrsToUpdate
  where
    getIdAtSqlValues = Output.atOutputerExisting (otIdAttributeType ot) idAtValue
                       :: ConvertResult [SqlValue]

-------------------------------------------------------------------------------
-- | Updates the given 'Attribute's of objects that
-- matches a given WHERE expression.
--
-- pures the number of updated rows. 'Nothing' means that this info is not
-- pureed by the DB backed.
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
                  -> NonEmpty.NonEmpty (Any (Attribute atConf dbTable))
                  -- ^ ORDER BY
                  -> DbConn.Monad (Maybe Integer)
update_attributes ot@(ObjectType {}) mbWhereExpr whereExprParams attrsToUpdate =
   do
     paramValuesForUpdates <- DbConn.toMonad getAttrsToUpdateSqlValues
     let paramValues = paramValuesForUpdates ++ whereExprParams
     numUpdatedRows <- SqlExec.update updateStmt paramValues
     pure $ if numUpdatedRows < 0
              then Nothing
              else Just numUpdatedRows
  where
    atsToUpdate    = getAts attrsToUpdate
    updateStmt     = SqlPlain.update_attributes ot atsToUpdate mbWhereExpr
    getAttrsToUpdateSqlValues = fmap
                                concat
                                (Output.aOutputsExisting (NonEmpty.toList attrsToUpdate))
                                :: ConvertResult [SqlValue]

    getAts :: NonEmpty.NonEmpty (Any (Attribute     atConf dbTable))
           -> NonEmpty.NonEmpty (Any (AttributeType atConf dbTable))
    getAts attrs = fmap (OmUtils.anyValueApply2 attrType) attrs


-------------------------------------------------------------------------------
-- | Executes a SQL statement where the WHERE expression is quality on
-- a single 'Attribute' given by it's type and value.
--
-- pures the number of rows affected by the statement.
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
           -> DbConn.Monad (Maybe Integer)
execForOne newSqlForWhereExpr
  whereEqAttrType@(AttributeType {})
  whereEqAttrValue
  sqlParamsBeforeWhereExpr
  =
  do
    whereEqAttrSqlValues <- DbConn.toMonad getAtSqlValues
    let sqlParams         = sqlParamsBeforeWhereExpr ++ whereEqAttrSqlValues
    execForWhereExpr newSqlForWhereExpr mbWhereExpr sqlParams
  where
    getAtSqlValues   = Output.atOutputerExisting whereEqAttrType whereEqAttrValue
                       :: ConvertResult [SqlValue]
    mbWhereExpr      = Utils.justAtEqPosParamExpr whereEqAttrType

-------------------------------------------------------------------------------
-- | Executes a SQL statement given a way to construct it from a given WHERE
-- expression, and the SQL parameters expected by the constructed SQL.
--
-- pures the number of rows affected by the statement.
-------------------------------------------------------------------------------
execForWhereExpr :: Sql.SQL_IDENTIFIER dbTable
                 => (Maybe (Sql.SqlExpr dbTable) -> Sql.SqlDmlStatement dbTable)
                 -> Maybe (Sql.SqlExpr dbTable)
                 -> [SqlValue]
                 -> DbConn.Monad (Maybe Integer)
execForWhereExpr newSqlForWhereExpr mbWhereExpr sqlParameters =
  SqlExec.execSql_numRowsMb (newSqlForWhereExpr mbWhereExpr) sqlParameters
