{-
Copyright 2013 Emil Karlén.

This file is part of Wilde.

Wilde is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Wilde is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Wilde.  If not, see <http://www.gnu.org/licenses/>.
-}

module TestResources.ReferenceObjectType
       (
         PkAndRefO

       , mkPkAndRefOts

       , ObjectTypeSetup(..)
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Wilde.Media.Database

import Wilde.Database.Sql (SQL_IDENTIFIER,SqlIdentifier,sqlIdentifier)

import Wilde.ApplicationConstruction.StandardServices as SS
import Wilde.ApplicationConstruction.StandardServices.Tools
import Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType
import Wilde.ApplicationConstruction.ObjectModel.ObjectType
import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDbConfig

import qualified Wilde.Driver.UserInteraction.StandardServiceLinkRenderer as SLR


-------------------------------------------------------------------------------
-- - object types with a reference -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Autoinc PK -
-------------------------------------------------------------------------------


type PkAndRefO t = t OtDbConfig.Configuration DdlAtAnnotation.Configuration PkAndRefTable () PrimaryKeyType PrimaryKeyType

data PkAndRefTable = Pk | Ref
                   deriving Show

instance SQL_IDENTIFIER PkAndRefTable where
  sqlIdentifier = show

type PkAndRefNative = ()

serviceLinkRenderer = SLR.renderer :: SLR.StandardServiceLinkRenderer

-- | Makes an ObjectTypeSetup for an 'ObjectType' that has a PK and a reference
-- to an 'ObjectType' of the same type.
mkPkAndRefOts :: SqlIdentifier
              -> PkAndRefO ObjectTypeSetup
              -> PkAndRefO ObjectTypeSetup
mkPkAndRefOts theTableName referenceTarget =
  objectTypeSetup ot title
  where
    title :: StyledTitle
    title = withNeutralWildeStyle theTableName

    databaseTable :: DatabaseTable
    databaseTable = DatabaseTable
                  {
                    tableName = theTableName
                  }
    rps :: PkAndRefO ReferencePresentationSpec
    rps = refPresSpec_4_showAt (otIdAttributeType (SS.objectType referenceTarget))

    ot :: PkAndRefO ObjectType
    ot = ObjectType
         {
           otCrossRefKey             = theTableName
         , otIdAttributeType         = pkAt
         , otNonIdAttributeTypes     = [Any refAt]
         , otToNative                = toNative
         , otConfiguration =
           OtDbConfig.Configuration
           {
             OtDbConfig.databaseTable               = databaseTable
           , OtDbConfig.getIdOfInsertedIntoDatabase = getIdOfInsertedIntoDatabase_fromMandatory
           }
         }
    nonIdAttributeTypes = [Any refAt]
    pkAt :: AttributeType DdlAtAnnotation.Configuration PkAndRefTable PrimaryKeyType PrimaryKeyType
    pkAt = at_PrimaryKeyType 5 Pk Nothing "PK"

    refAt :: AttributeType DdlAtAnnotation.Configuration PkAndRefTable (Maybe PrimaryKeyType) (Maybe PrimaryKeyType)
    refAt = at_ref_std_optional Ref referenceTarget rps Nothing

toNative :: ObjectToNativeFunction dbTable () idAtExisting idAtCreate
toNative = ObjectToNativeFunction $ const $ Right ()
