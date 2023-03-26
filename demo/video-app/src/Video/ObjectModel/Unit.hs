module Video.ObjectModel.Unit where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Database.Sql

import           Wilde.ObjectModel.ObjectModelUtils

import           Wilde.ApplicationConstruction.StandardServices.Tools as SS
import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType
import           Wilde.ApplicationConstruction.ObjectModel.ObjectType
import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS
import qualified Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectListFooters as Footers

import           Video.Ui.Style
import qualified Common.ObjectType.ObjectType as CommonOT
import qualified Common.ObjectType.AttributeType as CommonAT
import qualified Video.ObjectType.AttributeType as VideoAT


------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


tableName :: String
tableName = "unit"

data Table
  = Id
  | Media
  | Identity
  | Status
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

ots' :: StdAutoPkObjectTypeSetup_ddl Table NativeType
ots' = (SS.objectTypeSetup ot
        (withSingleClassStyle tableName "Storage media"))
       `SS.withObjectListDisplaySetup`
       (OLS.ObjectListDisplaySetup
        {
          OLS.displayAts                 = otNonIdAttributeTypes ot
        , OLS.orderByInDb                = [Any identity]
        , OLS.getMkFooterRowsConstructor = Footers.numberOfObjects
        }
       )

ot :: OObjectType
ot = CommonOT.ot_autoPk
     (CommonOT.databaseTable tableName)
           CommonOT.unit_toNative
           (CommonAT.primaryKey_dbAutogen Id)
           [ Any media
           , Any identity
           , Any status
           ]

rps :: OType ReferencePresentationSpec
rps = CommonOT.refPresSpec_4_string identity

media :: OptionalAttribute CommonAT.EnumNative
media = VideoAT.media_optional Media

identity :: MandatoryAttribute String
identity = CommonAT.name "Identity" Identity

status :: OptionalAttribute CommonAT.EnumNative
status = VideoAT.media_status_optional Status
