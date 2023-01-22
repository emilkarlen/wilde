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

import Wilde.Media.Database
import qualified Wilde.Media.Database.Monad as DbConn

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
                      -> DbConn.Monad a
convertOneRowOneValue converter errMsg [values] = convertOneValue converter errMsg values
convertOneRowOneValue converter errMsg rows =
  case rows of
    [] -> DbConn.throwErr $ DbNoRows      errMsg Nothing
    xs -> DbConn.throwErr $ DbTooManyRows errMsg (Just numRowsMismatch)
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
                -> DbConn.Monad a
convertOneValue converter errMsg [sqlValue] =
    case converter sqlValue of
      Left err -> DbConn.throwErr $ DbTranslationError $ AttributeTranslationError errMsg err
      Right x  -> return x
convertOneValue converter errMsg xs = DbConn.throwErr $ RecordTranslationError errMsg numValuesMismatch
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
