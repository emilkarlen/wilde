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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

-------------------------------------------------------------------------------
-- | Tools for constructing the object model of applications.
--
-- Construction of 'AttributeType's for some common types.
--
-- Currently, this module is a mess.  It should be divided into sub modules!
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.ObjectModel.ObjectType
       (
         -- * Re-exporting common closely related modules

         module Wilde.ApplicationConstruction.ObjectModel.AttributeType,

         -- * Standard types

         -- ** Types with a PK given explicitly

         StdObjectType,
         StdObject,

         StdObjectType_ddl,
         StdObject_ddl,
         
         -- ** Types with a PK autogen by the database

         -- *** Type synonyms helpers
         
         StdAutoPkO,
         StdAutoPkO_noAtAnn,
         StdAutoPkO_ddl,
         
         -- *** Standard types
         
         StdAutoPkObjectType,
         StdAutoPkObjectTypeSetup,
         StdAutoPkObject,

         StdAutoPkObjectTypeToNative,

         StdAutoPkObjectType_ddl,
         StdAutoPkObjectTypeSetup_ddl,
         StdAutoPkObject_ddl,

         -- * Id AttributeType utils
         
         getIdOfInsertedWhenEqualToCreate,
         
         -- * Reference Presentation Specifications

         refPresSpec_default,
         refPresSpec_4_at,
         refPresSpec_4_stringAt,
         refPresSpec_4_showAt,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Control.Monad.Error.Class

import Database.HDBC.Types
import Database.HDBC.ColTypes

import           Wilde.Utils.Empty

import           Wilde.Media.WildeValue
import           Wilde.Media.Database

import Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr
import Wilde.ObjectModel.ObjectModelUtils

import Wilde.WildeUi.StdValueTypes

import Wilde.ApplicationConstruction.StandardServices as StandardServices

import           Wilde.ApplicationConstruction.ObjectModel.StandardPrimaryKey
import           Wilde.ApplicationConstruction.ObjectModel.ReferenceAttributeType

import Wilde.ApplicationConstruction.ObjectModel.AttributeType

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.DdlAtAnnotation as DdlAtAnnotation
import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo as UiIoAndDbIo
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.Database as OtDbConfig


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - RefPresSpec -
-------------------------------------------------------------------------------


refPresSpec_default :: OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                    => PresentationAttributeTypeInfo        atConf dbTable e c
                    -> ReferencePresentationSpec     otConf atConf dbTable otNative idAtExisting idAtCreate
refPresSpec_default pati@(PresentationAttributeTypeInfo { patiAt = (AttributeType {}) }) =
  ReferencePresentationSpec
  {
    otpsPresentationString            = return . AnyVALUE . UnquotedStringValue . OmGsr.objOutputForIdAt
  , otpsPresentationStringOrder       = []
  , otpsPresentationAttributeTypeInfo = Any pati
  }

refPresSpec_4_stringAt :: AttributeType             atConf dbTable String a
                       -> ReferencePresentationSpec otConf atConf dbTable otNative idAtExisting idAtCreate
refPresSpec_4_stringAt at = refPresSpec_4_at id at

refPresSpec_4_showAt :: (Show idAtExisting)
                     => AttributeType                    atConf dbTable e a
                     -> ReferencePresentationSpec otConf atConf dbTable otNative idAtExisting idAtCreate
refPresSpec_4_showAt at@(AttributeType {}) = refPresSpec_4_at show at

refPresSpec_4_at :: (typeForExisting -> String)
                 -> AttributeType                    atConf dbTable typeForExisting a
                 -> ReferencePresentationSpec otConf atConf dbTable otNative idAtExisting idAtCreate
refPresSpec_4_at toString at@(AttributeType {}) =
  ReferencePresentationSpec
  {
    otpsPresentationString            = fmap (AnyVALUE . UnquotedStringValue) . f,
    otpsPresentationStringOrder       = [Any at],
    otpsPresentationAttributeTypeInfo = Any $ PresentationAttributeTypeInfo at toString
  }
  where
    f o = case lookupAttributeValue at o of
      Nothing      -> throwError $ ObjectModelError $ "Attribute not found: " ++ atCrossRefKey at
      Just convRes -> case convRes of
        Left convError -> throwError $ AttributeTranslationError "refPresSpec_4_stringAt" convError
        Right x        -> return (toString x)


-------------------------------------------------------------------------------
-- - Type synonyms -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - StdPkO -
-------------------------------------------------------------------------------


-- | An \"object-like\" type t, with the standard type of primary key ('PrimaryKeyType'),
-- whos value is input explicitly from UI.
type StdPkO t (otConf :: * -> * -> * -> * -> *) (atConf :: * -> * -> * -> *) dbTable native = t otConf atConf dbTable native PrimaryKeyType PrimaryKeyType

-- TODO The types below that uses StdPkO should have names StdPk...

-- | An 'Object' with the standard type of primary key ('PrimaryKeyType').
type StdObject              dbTable native = StdPkO Object          OtDbConfig.Configuration UiIoAndDbIo.ConfigurationSansAnnotation dbTable native
-- | A 'StdObject' with DDL info.
type StdObject_ddl          dbTable native = StdPkO Object          OtDbConfig.Configuration DdlAtAnnotation.Configuration           dbTable native
-- | An 'ObjectType' with the standard type of primary key ('PrimaryKeyType').
type StdObjectType          dbTable native = StdPkO ObjectType      OtDbConfig.Configuration UiIoAndDbIo.ConfigurationSansAnnotation dbTable native
type StdObjectType_ddl      dbTable native = StdPkO ObjectType      OtDbConfig.Configuration DdlAtAnnotation.Configuration           dbTable native
type StdObjectTypeSetup     dbTable native = StdPkO ObjectTypeSetup OtDbConfig.Configuration UiIoAndDbIo.ConfigurationSansAnnotation dbTable native
-- | An 'ObjectTypeSetup' with auto-inc key and DDL info.
type StdObjectTypeSetup_ddl dbTable native = StdPkO ObjectTypeSetup OtDbConfig.Configuration DdlAtAnnotation.Configuration           dbTable native


-------------------------------------------------------------------------------
-- - StdAutoPk -
-------------------------------------------------------------------------------


-- | An \"object-like\" type t, with the standard type of primary key ('PrimaryKeyType')
-- whos value is automatic (in some way).
type StdAutoPkO t (otConf :: * -> * -> * -> * -> *) (atConf :: * -> * -> * -> *) dbTable native = t otConf atConf dbTable native PrimaryKeyType (Maybe PrimaryKeyType)
type StdAutoPkO_noAtAnn t dbTable native = t OtDbConfig.Configuration UiIoAndDbIo.ConfigurationSansAnnotation dbTable native PrimaryKeyType (Maybe PrimaryKeyType)
type StdAutoPkO_ddl     t dbTable native = t OtDbConfig.Configuration DdlAtAnnotation.Configuration           dbTable native PrimaryKeyType (Maybe PrimaryKeyType)

-- | An 'ObjectType' with auto-inc key.
type StdAutoPkObject              dbTable native = StdAutoPkO_noAtAnn Object          dbTable native
-- | An 'ObjectType' with auto-inc key and DDL info.
type StdAutoPkObject_ddl          dbTable native = StdAutoPkO_ddl     Object          dbTable native
-- | An 'ObjectType' with auto-inc key.
type StdAutoPkObjectType          dbTable native = StdAutoPkO_noAtAnn ObjectType      dbTable native
-- | An 'ObjectType' with auto-inc key and DDL info.
type StdAutoPkObjectType_ddl      dbTable native = StdAutoPkO_ddl ObjectType          dbTable native
-- | An 'ObjectTypeSetup' with auto-inc key.
type StdAutoPkObjectTypeSetup     dbTable native = StdAutoPkO_noAtAnn ObjectTypeSetup dbTable native
-- | An 'ObjectTypeSetup' with auto-inc key and DDL info.
type StdAutoPkObjectTypeSetup_ddl dbTable native = StdAutoPkO_ddl ObjectTypeSetup     dbTable native

-- | The function that translates a 'StdAutoPkObjectType' to the \"native\" type.
type StdAutoPkObjectTypeToNative dbTable native =
  ObjectToNativeFunction dbTable native PrimaryKeyType (Maybe PrimaryKeyType)


-------------------------------------------------------------------------------
-- - getIdOfInsertedWhenEqualToCreate -
-------------------------------------------------------------------------------


-- | A 'GetIdOfInsertedIntoDatabase' for 'ObjectType's who's ID attribute is specified
-- as-is when created.
--
-- I.e., type for create and existing are equal, and the ID of an existing object
-- is the same as the value given when creating the object in the database.
--
getIdOfInsertedWhenEqualToCreate :: Database.GetIdOfInsertedIntoDatabase e e
getIdOfInsertedWhenEqualToCreate value dbOutput = return value
