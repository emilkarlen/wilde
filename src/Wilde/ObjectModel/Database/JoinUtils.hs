module Wilde.ObjectModel.Database.JoinUtils
       (
         -- * Translation of Object Model to SQL Join model

         newSqlEntity,
         newSqlAttribute,

         atColumnExprs,
         atColumnExprList,

         atExprEq,
         atExprEqMb,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.ObjectModel.ObjectModel

import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Database.HDBC

import Wilde.Media.Database

import qualified Wilde.Database.SqlJoin as Sql
import qualified Wilde.Database.SqlMisc as SqlMisc

import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Database.Utils as Utils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - translation of Object Model to SQL join elements -
-------------------------------------------------------------------------------


newSqlEntity :: Database.DATABASE_TABLE otConf
             => ObjectType otConf atConf dbTable otN idAE idAC -> Sql.Entity dbTable
newSqlEntity = Sql.newEntity . tableName . Database.otDatabaseTable

newSqlAttribute :: Database.COLUMN_NAMES atConf
                => AttributeType atConf dbTable e c -> Sql.Attribute dbTable e
newSqlAttribute = Sql.newAttribute . Utils.atColumnNames


-------------------------------------------------------------------------------
-- - attribute expressions -
-------------------------------------------------------------------------------


atColumnExprs :: Database.COLUMN_NAMES atConf
              => AttributeType atConf dbTable e c
              -> Sql.JoinMonad dbTable (NonEmpty.List (Sql.SqlExpr (Sql.BasedOn dbTable)))
atColumnExprs at@(AttributeType {}) = Sql.fieldExprs sqlAttrInBasedOn
  where
    sqlAttr          = newSqlAttribute at
    sqlAttrInBasedOn = Sql.includeFromBase sqlAttr

atColumnExprList :: Database.COLUMN_NAMES atConf
                 => AttributeType atConf dbTable e c
                 -> Sql.JoinMonad dbTable [Sql.SqlExpr (Sql.BasedOn dbTable)]
atColumnExprList = fmap NonEmpty.toList . atColumnExprs

-- | Gives an SQL expression for equality of an 'AttributeType' and a
-- value of it's type-for-existing.
--
-- The expression is in terms of positional parameters.  This function gives
-- a function that gives the values to supply for these.
atExprEq :: (Database.COLUMN_NAMES atConf
            ,Database.OUTPUT_FOR_EXISTING atConf
            )
         => AttributeType atConf dbTable e c
         -> (Sql.JoinMonad dbTable (Sql.SqlExpr (Sql.BasedOn dbTable)),
             e -> ConvertResult [SqlValue])
atExprEq at = (getExprs,getSqlValues)
  where
    getExprs = do
      columnExprs <- atColumnExprs at
      return $ SqlMisc.eqPosParamAndsNonEmpty columnExprs

    getSqlValues = Database.atOutputerExisting at

-- | A variant of 'atExprEq' that gives the expression as a 'Just' value
-- of a 'Maybe'.
atExprEqMb :: (Database.COLUMN_NAMES atConf
              ,Database.OUTPUT_FOR_EXISTING atConf
              )
           => AttributeType atConf dbTable e c
           -> (Sql.JoinMonad dbTable (Maybe (Sql.SqlExpr (Sql.BasedOn dbTable))),
               e -> ConvertResult [SqlValue])
atExprEqMb at = (fmap Just getExprs,getSqlValues)
  where
    (getExprs,getSqlValues) = atExprEq at
