-------------------------------------------------------------------------------
-- | Lookup of elements via the flag/option for Object Type.
--
-- There are enough of variants of this for having it separate from
-- 'Wilde.ApplicationTool.FlagLookup.ForMisc'.
-------------------------------------------------------------------------------
module Wilde.ApplicationTool.FlagLookup.ForObjectType
       (
         types_single,
         types_oneOrMore,

         typesWithDatabaseIo_single,
         typesWithDatabaseIo_oneOrMore,

         typesWithDll_single,
         typesWithDll_oneOrMore,

         setupsWithDll_single,
         setupsWithDll_oneOrMore,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.List.NonEmpty as NonEmpty

import Wilde.ObjectModel.ObjectModel

import Wilde.ApplicationTool.FlagLookup.Utils
import Wilde.ApplicationTool.FlagsAndOptions as FlagsAndOptions

import qualified Wilde.ApplicationConstruction.StandardServices as StandardServices
import qualified Wilde.ApplicationConstruction.ObjectTypeConfiguration.ObjectTypeWithAtDdlInformation as ObjectTypeWithAtDdlInformation

import qualified Wilde.ApplicationTool.ApplicationModel as ApplicationModel


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - Object Types -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Finds a single element identified by the Object Type flag/option.
--
-- pure value: see 'lookupSelection_single'.
-------------------------------------------------------------------------------
types_single :: ApplicationModel.OBJECT_TYPES m
             => m
             -> [Flag]
             -> IO (ApplicationModel.AnyAnyO ObjectType)
types_single =
  lookupByObjectTypeFlag_single withNothingSpecialConfig

-------------------------------------------------------------------------------
-- | Finds a single element identified by the Object Type flag/option.
--
-- pure value: see 'lookupSelection_single'.
-------------------------------------------------------------------------------
types_oneOrMore :: ApplicationModel.OBJECT_TYPES m
                => m
                -> [Flag]
                -> IO (NonEmpty.NonEmpty (ApplicationModel.AnyAnyO ObjectType))
types_oneOrMore =
  lookupByObjectTypeFlag_oneOrMore withNothingSpecialConfig

withNothingSpecialConfig :: ApplicationModel.OBJECT_TYPES m
                         => ElementTypeConfig m (ApplicationModel.AnyAnyO ObjectType)
withNothingSpecialConfig =
  ElementTypeConfig
  {
    elementTypeName       = "Object Type"
  , getAllElementsOfModel = ApplicationModel.objectTypes
  , getElementId          = ApplicationModel.idForAnyAny
  }


-------------------------------------------------------------------------------
-- - Database IO -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Finds a single element identified by the Object Type flag/option.
--
-- pure value: see 'lookupSelection_single'.
-------------------------------------------------------------------------------
typesWithDatabaseIo_single :: ApplicationModel.OBJECT_TYPES_WITH_DATABASE_IO_INFO m
                           => m
                           -> [Flag]
                           -> IO (ApplicationModel.AnyOWithDatabaseIo ObjectType)
typesWithDatabaseIo_single =
  lookupByObjectTypeFlag_single withDatabaseIoInfoConfig

-------------------------------------------------------------------------------
-- | Finds a single element identified by the Object Type flag/option.
--
-- pure value: see 'lookupSelection_single'.
-------------------------------------------------------------------------------
typesWithDatabaseIo_oneOrMore :: ApplicationModel.OBJECT_TYPES_WITH_DATABASE_IO_INFO m
                                        => m
                                        -> [Flag]
                                        -> IO (NonEmpty.NonEmpty (ApplicationModel.AnyOWithDatabaseIo ObjectType))
typesWithDatabaseIo_oneOrMore =
  lookupByObjectTypeFlag_oneOrMore withDatabaseIoInfoConfig

withDatabaseIoInfoConfig :: ApplicationModel.OBJECT_TYPES_WITH_DATABASE_IO_INFO m
                  => ElementTypeConfig m (ApplicationModel.AnyOWithDatabaseIo ObjectType)
withDatabaseIoInfoConfig =
  ElementTypeConfig
  {
    elementTypeName       = "Object Type with Database IO info"
  , getAllElementsOfModel = ApplicationModel.objectTypesWithDatabaseIoInfo
  , getElementId          = ApplicationModel.idForWithDatabaseIo
  }


-------------------------------------------------------------------------------
-- - DDL INFO -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Finds a single element identified by the Object Type flag/option.
--
-- pure value: see 'lookupSelection_single'.
-------------------------------------------------------------------------------
typesWithDll_single :: ApplicationModel.OBJECT_TYPES_WITH_DDL_INFO m
                    => m
                    -> [Flag]
                    -> IO (ObjectTypeWithAtDdlInformation.AnyO ObjectType)
typesWithDll_single =
  lookupByObjectTypeFlag_single withDllInfoConfig

-------------------------------------------------------------------------------
-- | Finds a single element identified by the Object Type flag/option.
--
-- pure value: see 'lookupSelection_single'.
-------------------------------------------------------------------------------
typesWithDll_oneOrMore :: ApplicationModel.OBJECT_TYPES_WITH_DDL_INFO m
                       => m
                       -> [Flag]
                       -> IO (NonEmpty.NonEmpty (ObjectTypeWithAtDdlInformation.AnyO ObjectType))
typesWithDll_oneOrMore =
  lookupByObjectTypeFlag_oneOrMore withDllInfoConfig

withDllInfoConfig :: ApplicationModel.OBJECT_TYPES_WITH_DDL_INFO m
                  => ElementTypeConfig m (ObjectTypeWithAtDdlInformation.AnyO ObjectType)
withDllInfoConfig =
  ElementTypeConfig
  {
    elementTypeName       = "Object Type with DDL info"
  , getAllElementsOfModel = ApplicationModel.objectTypesWithDdlInfo
  , getElementId          = ApplicationModel.idForWithDdlInfo
  }


-------------------------------------------------------------------------------
-- - Object Type Setups -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Finds a single element identified by the Object Type flag/option.
--
-- pure value: see 'lookupSelection_single'.
-------------------------------------------------------------------------------
setupsWithDll_single :: ApplicationModel.OBJECT_TYPE_SETUPS_WITH_DDL_INFO m
                     => m
                     -> [Flag]
                     -> IO (ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup)
setupsWithDll_single =
  lookupByObjectTypeFlag_single setupWithDllInfoConfig

-------------------------------------------------------------------------------
-- | Finds a single element identified by the Object Type flag/option.
--
-- pure value: see 'lookupSelection_single'.
-------------------------------------------------------------------------------
setupsWithDll_oneOrMore :: ApplicationModel.OBJECT_TYPE_SETUPS_WITH_DDL_INFO m
                        => m
                        -> [Flag]
                        -> IO (NonEmpty.NonEmpty (ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup))
setupsWithDll_oneOrMore =
  lookupByObjectTypeFlag_oneOrMore setupWithDllInfoConfig

setupWithDllInfoConfig :: ApplicationModel.OBJECT_TYPE_SETUPS_WITH_DDL_INFO m
                  => ElementTypeConfig m (ObjectTypeWithAtDdlInformation.AnyO StandardServices.ObjectTypeSetup)
setupWithDllInfoConfig =
  ElementTypeConfig
  {
    elementTypeName       = "Object Type Setup with DDL info"
  , getAllElementsOfModel = ApplicationModel.objectTypesWithStandardServiesWithDdlInfo
  , getElementId          = ApplicationModel.idForSetupWithDdlInfo
  }


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Finds a single element identified by the Object Type flag/option.
--
-- pure value: see 'lookupSelection_single'.
-------------------------------------------------------------------------------
lookupByObjectTypeFlag_single :: ElementTypeConfig m e
                              -> m
                              -> [Flag]
                              -> IO e
lookupByObjectTypeFlag_single =
  lookupObjectTypeForModel lookupSelection_single

-------------------------------------------------------------------------------
-- | Finds one or more element identified by the Object Type flag/option.
--
-- pure value: see 'lookupSelection_oneOrMore'.
-------------------------------------------------------------------------------
lookupByObjectTypeFlag_oneOrMore :: ElementTypeConfig m e
                                 -> m
                                 -> [Flag]
                                 -> IO (NonEmpty.NonEmpty e)
lookupByObjectTypeFlag_oneOrMore =
  lookupObjectTypeForModel lookupSelection_oneOrMore

-------------------------------------------------------------------------------
-- | Utility that makes it easy to call either
--
-- 'lookupSelection_single' or 'lookupSelection_oneOrMore'.
--
-- The 'FlagConfig' is always 'objectTypeFlagConfig'.
-------------------------------------------------------------------------------
lookupObjectTypeForModel :: (ElementTypeAndFlagConfig m e
                             -> m
                             -> [Flag]
                             -> IO r)
                            -- ^ Does the work with complete config.
                          -> ElementTypeConfig m e
                          -- ^ Config that may vary.
                          -> m
                          -> [Flag]
                          -> IO r
lookupObjectTypeForModel doItWithCompleteConfig elementTypeConfig =
  doItWithCompleteConfig completeConfig
  where
    completeConfig =
      ElementTypeAndFlagConfig
      {
        elementTypeConfig              = elementTypeConfig
      , flagForSelectingASingleElement = objectTypeFlagConfig
      }

-------------------------------------------------------------------------------
-- | Configuration for the Object Type flag/option.
-------------------------------------------------------------------------------
objectTypeFlagConfig :: FlagConfig
objectTypeFlagConfig =
  FlagConfig
  {
    flagOptionName  = FlagsAndOptions.optionNameObjectType
  , flagLookup      = lookupObjectTypeFlagValue
  }

-------------------------------------------------------------------------------
-- | Looks up the value of the flag for \"Object Type\".
-------------------------------------------------------------------------------
lookupObjectTypeFlagValue :: [Flag] -> Maybe String
lookupObjectTypeFlagValue flags =
  case flagOptional flagIsObjectType flags of
    Nothing                -> Nothing
    Just (TheObjectType s) -> Just s
    _                      -> implError "TheObjectType"
