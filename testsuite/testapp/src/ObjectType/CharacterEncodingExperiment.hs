module ObjectType.CharacterEncodingExperiment where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Database.Sql

import Wilde.Media.Database

import Wilde.ObjectModel.ObjectModelUtils

import Wilde.Driver.Database.MySQL.ApplicationObjectModelTools as ToolsMySql

import Wilde.ApplicationConstruction.ObjectModel.ObjectType
import Wilde.ApplicationConstruction.StandardServices.Tools
import Wilde.ApplicationConstruction.StandardServices as StandardServices

import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDbConfig
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation as ObjectTypeWithAtDdlInformation

import AttributeType


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Table = Id | String_

instance SQL_IDENTIFIER Table where
  sqlIdentifier Id      = "id"
  sqlIdentifier String_ = "string"

databaseTable :: DatabaseTable
databaseTable = DatabaseTable "character_encoding_experiment"

type NativeType = (PrimaryKeyType,Maybe String)

title :: WildeTitle
title = wildeStyling
        (WildeStyle ["character_encoding_experiment"])
        "Character Encoding Experiment"

aots :: ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup
aots = ObjectTypeWithAtDdlInformation.AnyO $ ots

rps :: ReferencePresentationSpec OtDbConfig.Configuration DdlAtAnnotation.Configuration Table otNative idAtExisting idAtCreate
rps = refPresSpec_default $
      PresentationAttributeTypeInfo atName (maybe "<anonymous>" id)

ots :: StdAutoPkObjectTypeSetup_ddl Table NativeType
ots = objectTypeSetup ot title
      `withModifiedObjectListDisplaySetup`
      (setOrderByInDb [Any atId])

ot :: StdAutoPkObjectType_ddl Table NativeType
ot = ToolsMySql.ot_PrimaryKey_dbAutogen_MySql
     databaseTable
     toNative
     atId
     nonIdAttributeTypes

atId :: StdAutoPkPkAttributeType_ddl Table
atId = at_PrimaryKey_dbAutogen 10 Id

atName :: PlainAttributeType_optional_ddl Table String
atName = at_Name_optional
         "String åäö"
         String_

nonIdAttributeTypes :: [Any (AttributeType DdlAtAnnotation.Configuration Table)]
nonIdAttributeTypes = [Any atName]

toNative :: ObjectToNativeFunction Table NativeType PrimaryKeyType (Maybe PrimaryKeyType)
toNative = ObjectToNativeFunction f
  where
    f o =
      let
        pkValue = attrValue $ oIdAttribute o
      in
       case oNonIdAttributes o of
         [Any aString@(Attribute {})] ->
           do
             mbString <- doOtnUnhide $ attrValue aString
             pure (pkValue,mbString)
         attrs -> numAttributesError2 attrs 1
