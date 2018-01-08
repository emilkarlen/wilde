module Video.ObjectModel.Episode where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Database.Sql

import           Wilde.ObjectModel.ObjectModelUtils

import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType
import           Wilde.ApplicationConstruction.ObjectModel.ObjectType

import qualified Common.ObjectType.ObjectType as CommonOT
import qualified Common.ObjectType.AttributeType as CommonAT
import qualified Video.ObjectType.AttributeType as VideoAT


------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


tableName :: String
tableName = "episode"

data Table = Id
           | Season_Id
           | Number
           | Status
           | Title
           | Director_Id
           | Url
           deriving Show

instance SQL_IDENTIFIER Table where
  sqlIdentifier x = show x

type NativeType = ()

type OType t = StdAutoPkO_ddl t Table NativeType

type OObjectType = OType ObjectType
type OObject     = OType Object

type MandatoryAttribute a = PlainAttributeType_ddl           Table a
type OptionalAttribute  a = PlainAttributeType_optional_ddl  Table a
type OptionalReference    = StdRefAttributeType_optional_ddl Table

rps :: OType ReferencePresentationSpec
rps = CommonOT.refPresSpec_4_optionalString name

number :: MandatoryAttribute Word32
number = CommonAT.word32 "Nummer" Number

status :: OptionalAttribute CommonAT.EnumNative
status = VideoAT.status_optional Status

name :: OptionalAttribute String
name = CommonAT.name_optional "Titel" Title

url :: OptionalAttribute String
url = CommonAT.href_optional "Länk" Url
