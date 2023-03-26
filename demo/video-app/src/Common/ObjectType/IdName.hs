module Common.ObjectType.IdName
(
  IdNameObjectType.IdNameType,
  IdNameObjectType.Table,
  IdNameObjectType.IdNamePresStrSpec_ddl,
  IdNameObjectType.IdNameObjectTypeSetup_ddl,
  rps_and_ots_stdPkName,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Database.Sql

import           Wilde.WildeUi.UiPrimitives

import qualified Wilde.ApplicationConstruction.ObjectModel.IdNameObjectType as IdNameObjectType

import qualified Wilde.Driver.Database.MySQL.ApplicationObjectModelTools as ToolsMySql

import qualified Common.ObjectType.AttributeType as MyAT


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Constructor of 'ObjectType's with auto-gen PK via the database.
-- ot_autoPk = ToolsMySql.ot_PrimaryKey_dbAutogen_MySql
rps_and_ots_stdPkName :: Title   -- ^ Title of Object Type
                      -> SqlIdentifier -- ^ Database Table Name
                      -> (IdNameObjectType.IdNamePresStrSpec_ddl     IdNameObjectType.Table,
                          IdNameObjectType.IdNameObjectTypeSetup_ddl IdNameObjectType.Table)
rps_and_ots_stdPkName titleOt tableName =
  ToolsMySql.ots_and_rps_IdName_dbAutogen_MySql
  (mkIdNameConfig titleOt tableName)

mkIdNameConfig :: Title
               -> SqlIdentifier -- ^ Database Table Name
               -> ToolsMySql.IdNameOtConfiguration IdNameObjectType.Table
mkIdNameConfig objectTypeTitle dbTableName =
  ToolsMySql.IdNameOtConfiguration
  {
    ToolsMySql.objectTypeTitle = withNeutralWildeStyle objectTypeTitle
  , ToolsMySql.nameTitle       = "Namn"
  , ToolsMySql.maxSize         = maxSize_name
  , ToolsMySql.inputWidth      = inputWidth_name
  , ToolsMySql.dbTableName     = dbTableName
  , ToolsMySql.colPk           = IdNameObjectType.Id
  , ToolsMySql.colName         = IdNameObjectType.Name
  , ToolsMySql.dbIo_string     = MyAT.my_dbIo_string
  }

maxSize_name :: Int
maxSize_name = 50

inputWidth_name :: Int
inputWidth_name = 40
