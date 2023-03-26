module AttributeType where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------

import qualified Data.ByteString.Char8 as Char8

import Wilde.Database.Sql

import Wilde.Media.Database

import Wilde.ApplicationConstruction.ObjectModel.ObjectType

import Wilde.ApplicationConstruction.Database.DatabaseColumnTypes (dbIo_mkOptional, numberOfSqlValuesError, invalidSqlValuesError)
import           Wilde.WildeUi.UiPrimitives (Title)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


inputWidth :: Int
inputWidth = 5

maxSize_name :: Int
maxSize_name = 50

inputWidth_name :: Int
inputWidth_name = 20

at_Name_optional :: SQL_IDENTIFIER table
                 => Title
                 -> table
                 -> PlainAttributeType_optional_ddl table String
at_Name_optional title col = at_String_optional
                            dbIo_string_optional
                            maxSize_name
                            inputWidth_name
                            col
                            Nothing
                            title

dbIo_string_optional :: DatabaseIo (Maybe String)
dbIo_string_optional = dbIo_mkOptional dbIo_string


dbIo_string :: DatabaseIo String
dbIo_string =
  DatabaseIo
  {
    dbOutputer = dbO_string
  , dbInputer  = dbI_string
  }

dbI_string :: DatabaseInputer String
dbI_string [sqlValue] = case sqlValue of
  (SqlString x)     -> pure x
  (SqlByteString x) -> pure $ Char8.unpack x
  sqlValue          -> Left $
                       invalidSqlValuesError
                       "String"
                       ["SqlString","SqlByteString"]
                       sqlValue
dbI_string sqlValues = Left $ numberOfSqlValuesError "String" sqlValues

dbO_string :: DatabaseOutputer String
dbO_string x = pure [SqlByteString $ Char8.pack x]
