module Common.ObjectType.ObjectType
       (
         unit_toNative,
         ot_autoPk,
         databaseTable,
         refPresSpec_4_string,
         refPresSpec_4_optionalString,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.ObjectModel.ObjectModelUtils

import           Wilde.ApplicationConstruction.ObjectModel.ObjectType
import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType

import Wilde.Media.Database

import qualified Wilde.Driver.Database.MySQL.ApplicationObjectModelTools as ToolsMySql


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Constructor of 'ObjectType's with auto-gen PK via the database.
ot_autoPk = ToolsMySql.ot_PrimaryKey_dbAutogen_MySql

-- | A 'StdAutoPkObjectTypeToNative' for 'ObjectType's that
-- has the unit type as native type.
unit_toNative :: StdAutoPkObjectTypeToNative dbTable ()
unit_toNative = ObjectToNativeFunction $ const (return ())

databaseTable :: String -> DatabaseTable
databaseTable theTableName =
  DatabaseTable
  {
    tableName = theTableName  
  }

refPresSpec_4_optionalString
  :: AttributeType atConf dbTable (Maybe String) a
     -> ReferencePresentationSpec
          otConf atConf dbTable otNative idAtExisting idAtCreate
refPresSpec_4_optionalString = refPresSpec_4_at (maybe "<namnlös>" id)

refPresSpec_4_string
  :: AttributeType atConf dbTable String a
  -> ReferencePresentationSpec otConf atConf dbTable otNative idAtExisting idAtCreate
refPresSpec_4_string = refPresSpec_4_stringAt
