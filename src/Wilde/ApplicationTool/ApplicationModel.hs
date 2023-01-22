{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}

-------------------------------------------------------------------------------
-- | Definitions of the \"application model\" - the information about a
-- Wilde application that the Application Tool provides commands for.
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.ApplicationModel
       (
         -- * Encapsulation of Object/Object Type-like elements

         AnyOWithDatabaseIo(..),

         AnyAnyO(..),
         idForAnyAny,
         anyAnyApply,

         idForWithDatabaseIo,
         idForWithDdlInfo,

         idForSetupWithDdlInfo,


         -- * Classes for the application model

         OBJECT_TYPES(..),
         OBJECT_TYPES_WITH_DATABASE_IO_INFO(..),
         OBJECT_TYPES_WITH_DDL_INFO(..),
         OBJECT_TYPE_SETUPS_WITH_DDL_INFO(..),

         -- * Implementation of an application model

         ObjectModel(..),

         -- * Utilities

         allWithDdlInfo,
         stripDdlInfo,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.ObjectModel.Database as DatabaseClasses
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import           Wilde.ObjectModel.ObjectModelUtils

import qualified Wilde.ApplicationConstruction.StandardServices as StandardServices
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation as ObjectTypeWithAtDdlInformation


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | A variant of 'Any' for 'ObjectType's and 'Object's.
data AnyOWithDatabaseIo t =
  forall (otConf :: * -> * -> * -> * -> *) atConf dbTable otNative idAtExisting idAtCreate .
  (DatabaseClasses.DATABASE_TABLE otConf
  ,DatabaseClasses.DATABASE_IO atConf
  ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
  )
  => AnyOWithDatabaseIo (t otConf atConf dbTable otNative idAtExisting idAtCreate)

idForWithDdlInfo :: ObjectTypeWithAtDdlInformation.AnyO ObjectType -> String
idForWithDdlInfo (ObjectTypeWithAtDdlInformation.AnyO ot) = otCrossRefKey ot

idForSetupWithDdlInfo :: ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup
                      -> String
idForSetupWithDdlInfo (ObjectTypeWithAtDdlInformation.AnyO setup) = otCrossRefKey $ StandardServices.objectType setup

idForWithDatabaseIo :: AnyOWithDatabaseIo ObjectType -> String
idForWithDatabaseIo (AnyOWithDatabaseIo ot) = otCrossRefKey ot

-- | A variant of 'Any' for 'ObjectType's and 'Object's.
data AnyAnyO t =
  forall (otConf :: * -> * -> * -> * -> *) (atConf :: * -> * -> * -> *) dbTable otNative idAtExisting idAtCreate .
  AnyAnyO (t otConf atConf dbTable otNative idAtExisting idAtCreate)

idForAnyAny :: AnyAnyO ObjectType -> String
idForAnyAny (AnyAnyO ot) = otCrossRefKey ot

anyAnyApply :: (forall otConf atConf dbTable otNative idAtE idAtC .
                t otConf atConf dbTable otNative idAtE idAtC -> a)
            -> AnyAnyO t
            -> a
anyAnyApply f (AnyAnyO x) = f x


class OBJECT_TYPES a where
  objectTypes :: a -> [AnyAnyO ObjectType]

class OBJECT_TYPES a => OBJECT_TYPES_WITH_DATABASE_IO_INFO a where
  objectTypesWithDatabaseIoInfo :: a -> [AnyOWithDatabaseIo ObjectType]

class OBJECT_TYPES_WITH_DATABASE_IO_INFO a => OBJECT_TYPES_WITH_DDL_INFO a where
  objectTypesWithDdlInfo :: a -> [ObjectTypeWithAtDdlInformation.AnyO ObjectType]

class OBJECT_TYPES_WITH_DDL_INFO a => OBJECT_TYPE_SETUPS_WITH_DDL_INFO a where
  objectTypesWithStandardServiesWithDdlInfo :: a -> [ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup]

data ObjectModel =
  ObjectModel
  {
    objectTypesWithStandardServies    :: [ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup]
  , objectTypesWithoutStandardServies :: [ObjectTypeWithAtDdlInformation.AnyO ObjectType]
  }

allWithDdlInfo :: ObjectModel -> [ObjectTypeWithAtDdlInformation.AnyO ObjectType]
allWithDdlInfo (ObjectModel otss ots) = map (ObjectTypeWithAtDdlInformation.anyODdlApply2 StandardServices.objectType) otss ++ ots

stripDdlInfo :: ObjectTypeWithAtDdlInformation.AnyO ObjectType -> AnyAnyO ObjectType
stripDdlInfo (ObjectTypeWithAtDdlInformation.AnyO ot) = AnyAnyO ot

instance OBJECT_TYPES ObjectModel where
  objectTypes = map stripDdlInfo . allWithDdlInfo

instance OBJECT_TYPES_WITH_DATABASE_IO_INFO ObjectModel where
  objectTypesWithDatabaseIoInfo = map changeWrapper . allWithDdlInfo
    where
      changeWrapper (ObjectTypeWithAtDdlInformation.AnyO x) = AnyOWithDatabaseIo x

instance OBJECT_TYPES_WITH_DDL_INFO ObjectModel where
  objectTypesWithDdlInfo = allWithDdlInfo

instance OBJECT_TYPE_SETUPS_WITH_DDL_INFO ObjectModel where
  objectTypesWithStandardServiesWithDdlInfo = objectTypesWithStandardServies
