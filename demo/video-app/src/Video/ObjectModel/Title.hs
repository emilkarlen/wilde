module Video.ObjectModel.Title where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Database.Sql

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
tableName = "title"

data Table
  = Id
  | Title
  | Genre_Id
  | Animated
  | Status
  | Country_Id
  | Year
  | Length
  | Director_Id
  | Url
  | Comment
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
rps = CommonOT.refPresSpec_4_string name

name :: MandatoryAttribute String
name = CommonAT.name "Title" Title

animated :: MandatoryAttribute Bool
animated = CommonAT.bool "Animated" Animated

status :: OptionalAttribute CommonAT.EnumNative
status = VideoAT.status_optional Status

year :: OptionalAttribute Word32
year = VideoAT.year_optional Year

length :: OptionalAttribute Word32
length = CommonAT.word32_optional "Length" Length

url :: OptionalAttribute String
url = CommonAT.href_optional "Url" Url

comment :: OptionalAttribute String
comment = CommonAT.text_optional "Comment" Comment
