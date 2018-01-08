module Video.ObjectModel.Director where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Database.Sql

import           Wilde.ObjectModel.ObjectModelUtils

import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType
import           Wilde.ApplicationConstruction.ObjectModel.ObjectType

import qualified Common.ObjectType.ObjectType as CommonOT
import qualified Common.ObjectType.AttributeType as CommonAT
-- import qualified Pills.ObjectType.AttributeType as PillsAT


------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


tableName :: String
tableName = "director"

data Table = Id
           | Name
           | Country_id
           | Url
           deriving Show

instance SQL_IDENTIFIER Table where
  sqlIdentifier x = show x

type NativeType = ()

type OType t = StdAutoPkO_ddl t Table NativeType

type OObjectType = OType ObjectType
type OObject     = OType Object

rps :: OType ReferencePresentationSpec
rps = CommonOT.refPresSpec_4_string name

name :: PlainAttributeType_ddl Table String
name = CommonAT.name "Namn" Name

url :: PlainAttributeType_optional_ddl Table String
url = CommonAT.href_optional "Länk" Url
