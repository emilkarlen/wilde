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

-- | Utilities related to database SQL generation.
module Wilde.ObjectModel.Database.Utils
       (
         COLUMN_NAMES(..),

         ObjectTranslationSetup(..),
         inputPlainObjectSetup,
         
         getOrderByCols,
         
         atColumnList,
         atColumnListAny,
         
         atColumnNames,
         atColumnNameList,
         
         aEqExpr,
         atEqPosParamExpr,
         justAtEqPosParamExpr,
         
         otIdAtEqPosParamExpr,
         justOtIdAtEqPosParamExpr,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Convertible.Base

import Wilde.ObjectModel.ObjectModelUtils

import Wilde.Database.Sql
import Wilde.Database.SqlMisc
import Database.HDBC

import Wilde.Utils.Utils
import qualified Wilde.Utils.NonEmptyList as NonEmpty

import Wilde.Media.Database

import Wilde.ObjectModel.Database


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


getOrderByCols :: COLUMN_NAMES atConf 
               => [Any (AttributeType atConf dbTable)]
               -> [dbTable]
getOrderByCols orderBy = map columnName $ concatMap atColumnListAny orderBy

-- | Non-empty list of columns for an 'AttributeType'.
atColumnNames :: COLUMN_NAMES atConf
              => AttributeType atConf dbTable typeForExisting typeForCreate
              -> NonEmpty.List dbTable
atColumnNames = fmap columnName . atColumns

-- | Non-empty list of columns for an 'AttributeType'.
atColumnNameList :: COLUMN_NAMES atConf
                 => AttributeType atConf dbTable typeForExisting typeForCreate
                 -> [dbTable]
atColumnNameList = map columnName . atColumnList


-------------------------------------------------------------------------------
-- | List of an 'AttributeType's database columns.
-------------------------------------------------------------------------------
atColumnList :: COLUMN_NAMES atConf
             => AttributeType atConf dbTable typeForExisting typeForCreate
             -> [DatabaseColumn dbTable]
atColumnList = NonEmpty.toList . atColumns

-------------------------------------------------------------------------------
-- | List of an 'AttributeType's database columns.
-------------------------------------------------------------------------------
atColumnListAny :: COLUMN_NAMES atConf
                => Any (AttributeType atConf dbTable)
                -> [DatabaseColumn dbTable]
atColumnListAny (Any at) = atColumnList at

-- | Information about the 'AttributeType's and database columns of
-- an 'ObjectType' for reading an object from a database record.
data ObjectTranslationSetup atConf dbTable otNative idAtExisting idAtCreate =
  ObjectTranslationSetup
  {
    setupIdAt            :: AttributeType atConf dbTable idAtExisting idAtCreate
  , setupIdAtColumns     :: [DatabaseColumn dbTable]
  , setupNonIdAts        :: [Any (AttributeType atConf dbTable)]
  , setupNonIdAtsColumns :: [[DatabaseColumn dbTable]]
  }

-- | Helper method for getting the information needed by 'translatePlainObjectUtil'.
inputPlainObjectSetup :: COLUMN_NAMES atConf
                      => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                      -> ObjectTranslationSetup atConf dbTable otNative idAtExisting idAtCreate
inputPlainObjectSetup ot =
  ObjectTranslationSetup
  {
    setupIdAt            = atId
  , setupIdAtColumns     = dbColsIdAt
  , setupNonIdAts        = atNonIds
  , setupNonIdAtsColumns = dbColsNonIdAts
  }
  where
    atId           = otIdAttributeType ot
    atNonIds       = otNonIdAttributeTypes ot
    dbColsIdAt     = atColumnList atId
    dbColsNonIdAts = mapAttributeTypeAnyValue atColumnList atNonIds

-------------------------------------------------------------------------------
-- | Constructs an expression: col1 = % AND col2 = % ...
-- for all columns of the given 'AttributeType'.
--
-- The expression expects values for the positional parameters,
-- which are as many as the number of columns of the 'AttributeType'.
-------------------------------------------------------------------------------
atEqPosParamExpr :: COLUMN_NAMES atConf 
                 => AttributeType atConf dbTable typeForExisting typeForCreate
                 -> SqlExpr dbTable
atEqPosParamExpr = eqFieldPosParamAndsNonEmpty . atColumnNames

-------------------------------------------------------------------------------
-- | Gives 'Just' 'atEqPosParamExpr'.
--
-- Utility for skipping the 'Just' in places where a 'Maybe' is expected.
-------------------------------------------------------------------------------
justAtEqPosParamExpr :: COLUMN_NAMES atConf 
                     => AttributeType atConf dbTable typeForExisting typeForCreate
                     -> Maybe (SqlExpr dbTable)
justAtEqPosParamExpr = Just . atEqPosParamExpr

-------------------------------------------------------------------------------
-- | An expression for equals on all columns of the ID 'AttributeType'.
-------------------------------------------------------------------------------
otIdAtEqPosParamExpr :: COLUMN_NAMES atConf
                     => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                     -> SqlExpr dbTable
otIdAtEqPosParamExpr = atEqPosParamExpr . otIdAttributeType

-------------------------------------------------------------------------------
-- | Gives 'Just' 'otIdAtEqPosParamExpr'.
--
-- Utility for skipping the 'Just' in places where a 'Maybe' is expected.
-------------------------------------------------------------------------------
justOtIdAtEqPosParamExpr :: COLUMN_NAMES atConf
                         => ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
                         -> Maybe (SqlExpr dbTable)
justOtIdAtEqPosParamExpr = Just . otIdAtEqPosParamExpr

-------------------------------------------------------------------------------
-- | An equality expression for an 'Attribute'.
-------------------------------------------------------------------------------
aEqExpr :: (OUTPUT_FOR_EXISTING atConf
           ,COLUMN_NAMES atConf)
        => Attribute atConf dbTable typeForExisting typeForCreate
        -> ConvertResult ((SqlExpr dbTable),[SqlValue])
aEqExpr (Attribute at value _) =
  do
    columnValues <- outputExisting value
    if (length columnValues /= length columnNames)
      then
      let
        mismatch = Mismatch (length columnValues) (length columnNames)
        msg      = "Num values for attribute as SqlValue not equal to num columns: " ++ show mismatch
      in
       Left $ ConvertError "" "" "" msg
      else return $ (mkEqExpr $ zip columnNames columnValues,columnValues)
   where
    columnNames    = map columnName $ NonEmpty.toList $ atColumns at
    outputExisting = atOutputerExisting at
     -- Transforms columns and values to an equality expression.
     -- The input list is non-empty.
    mkEqExpr :: [(dbTable,SqlValue)] -> SqlExpr dbTable
    mkEqExpr xs = eqPosParamAnds $ map fst xs -- ands $ map eqExpr xs
