module Video.ObjectModel.Recording where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Database.Sql

import           Wilde.ObjectModel.ObjectModelUtils

import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType
import           Wilde.ApplicationConstruction.ObjectModel.ObjectType

import qualified Common.ObjectType.ObjectType as CommonOT
import qualified Common.ObjectType.AttributeType as CommonAT


------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


tableName :: String
tableName = "recording"

data Table = Id
           | Unit_Id
           | Title_Id
           | Season_Id
           | Episode_Id
           | Rec_Size
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

-- rps :: OType ReferencePresentationSpec
-- rps =CommonOT.refPresSpec_4_string name

rec_size :: OptionalAttribute Double
rec_size = CommonAT.double_optional "Gb" Rec_Size
