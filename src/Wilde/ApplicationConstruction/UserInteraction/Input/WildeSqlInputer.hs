-------------------------------------------------------------------------------
-- | A function for inputing an SQL expression.
--
-- Contains the \"main\" function for parsing a \"WildeSql\" expression
-- from a string.
--
-- This module uses the following modules as helpers:
--
-- * "Wilde.ApplicationConstruction.StandardServices.SqlExprParser"
--
-- * "Wilde.ApplicationConstruction.StandardServices.ParsedSqlExpr2WildeSqlExpr"
--
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.Input.WildeSqlInputer
       (
         expression_mandatory,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Char

import qualified Data.List.NonEmpty as NonEmpty

import Wilde.Database.SqlJoin

import qualified Wilde.Media.ElementSet as ES

import Wilde.Media.Database (DatabaseColumn(..))

import Wilde.ObjectModel.ObjectModel
import Wilde.ObjectModel.ObjectModelUtils

import qualified Wilde.ApplicationConstruction.UserInteraction.Input.SqlExprParser as ExprParser
import qualified Wilde.ApplicationConstruction.UserInteraction.Input.ParsedSqlExpr2WildeSqlExpr as Translate

import qualified Wilde.ObjectModel.Database as DatabaseClasses


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inputs an SQL expression "based" on the database table of a given
-- 'ObjectType'.
-------------------------------------------------------------------------------
expression_mandatory :: DatabaseClasses.COLUMN_NAMES atConf
                     => ObjectType otConf atConf dbTable otNative idAtE idAtC
                     -> ES.Parser String (SqlExpr dbTable)
expression_mandatory ot string =
  case ExprParser.parseWithListedColumns tableColumnInfos string of
    Left parseError -> Left ES.InvalidSyntax -- (show parseError)
    Right expr      -> pure (Translate.translate expr)
  where
    tableColumnInfos = columnInfosForParsing ot

-------------------------------------------------------------------------------
-- | Extracts the information about columns of the database table of
-- an 'ObjectType', that is needed for parsing with "SqlExprParser".
-------------------------------------------------------------------------------
columnInfosForParsing :: DatabaseClasses.COLUMN_NAMES atConf
                      => ObjectType otConf atConf dbTable native idAtE idAtC
                      -> [(String,dbTable)]
columnInfosForParsing ot = cols (otIdAttributeType ot) ++
                           concatMap (anyValueApply cols) (otNonIdAttributeTypes ot)
  where
    cols :: DatabaseClasses.COLUMN_NAMES atConf
         => AttributeType atConf dbTable typeForExisting typeForCreate
         -> [(String,dbTable)]
    cols at@(AttributeType {}) =
      (
        NonEmpty.toList
       . fmap (\(DatabaseColumn col) -> (map toLower $ sqlIdentifier col,col))
       . DatabaseClasses.atColumns
      )
      at
