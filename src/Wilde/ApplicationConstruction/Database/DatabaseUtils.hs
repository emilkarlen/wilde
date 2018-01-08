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
-- | Utilities related to the database.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.Database.DatabaseUtils
       (
         convertOneRowOneValue,
         convertOneValue,

         -- * Functions for modifying database column info

         -- modifyAtColumnName,
         modifyAtDatabaseInfo,
         modifyDatabaseColumn,

         -- * Utils

         anyOWithDdlInfo2AnyO,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Database.HDBC

import Wilde.Utils.Utils

import Wilde.Media.Database.Monad

import Wilde.ObjectModel.ObjectModelUtils

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDatabase
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation as ObjectTypeWithAtDdlInformation
import Wilde.ApplicationConstruction.Database.AttributeTypeDatabaseInfo (AttributeTypeDatabaseConfigForExisting(..))


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Converts rows that is expected to be: a single row with a single 'SqlValue'
-------------------------------------------------------------------------------
convertOneRowOneValue :: (SqlValue -> ConvertResult a) -- ^ Converts from SQL to \"Haskell\"
                      -> String -- ^ Error message string
                      -> [[SqlValue]]
                      -- ^ Input from database. This is expected to be exactly one
                      -- row with exactly one value.
                      -> DatabaseMonad a
convertOneRowOneValue converter errMsg [values] = convertOneValue converter errMsg values
convertOneRowOneValue converter errMsg rows =
  case rows of
    [] -> throwErr $ DbNoRows      errMsg Nothing
    xs -> throwErr $ DbTooManyRows errMsg (Just numRowsMismatch)
  where
    numRowsMismatch =
      Mismatch
      {
        actual   = length rows
      , expected = 1
      }

-------------------------------------------------------------------------------
-- | Converts a row that is expected to be a single 'SqlValue'
-------------------------------------------------------------------------------
convertOneValue :: (SqlValue -> ConvertResult a) -- ^ Converts from SQL to \"Haskell\"
                -> String -- ^ Error message string
                -> [SqlValue]
                -- ^ Input from database. This is expected to be exactly one value.
                -> DatabaseMonad a
convertOneValue converter errMsg [sqlValue] =
    case converter sqlValue of
      Left err -> throwErr $ DbTranslationError $ AttributeTranslationError errMsg err
      Right x  -> return x
convertOneValue converter errMsg xs = throwErr $ RecordTranslationError errMsg numValuesMismatch
  where
    numValuesMismatch =
      Mismatch
      {
        actual   = show (length xs) ++ " values"
      , expected = "1 value"
      }


-------------------------------------------------------------------------------
-- - Builders -
-------------------------------------------------------------------------------

modifyAtDatabaseInfo :: (dbTable -> dbTable)
                     -> AttributeTypeDatabaseConfigForExisting dbTable typeForExisting
                     -> AttributeTypeDatabaseConfigForExisting dbTable typeForExisting
modifyAtDatabaseInfo f x =
  x { atdbioeStructure = fmap (modifyDatabaseColumn f) (atdbioeStructure x) }

modifyDatabaseColumn :: (dbTable -> dbTable)
                     -> DatabaseColumn dbTable
                     -> DatabaseColumn dbTable
modifyDatabaseColumn f x =
  x { columnName = f (columnName x) }


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


anyOWithDdlInfo2AnyO :: ObjectTypeWithAtDdlInformation.AnyO t 
                     -> AnyO (t OtDatabase.Configuration DdlAtAnnotation.Configuration)
anyOWithDdlInfo2AnyO (ObjectTypeWithAtDdlInformation.AnyO x) = AnyO x
