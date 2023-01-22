{-# LANGUAGE ExistentialQuantification #-}

-------------------------------------------------------------------------------
-- | An annotation for AttributeTypes that provides
--
-- * User Interaction IO
--
-- * Database IO
--
-- Import this module qualified.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo
       (
         module Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo.Configuration,

         EmptyAtAnnotation,
         mkEmptyAtAnnotation,

         ConfigurationSansAnnotation,

         -- * Utilities

         atUiIoForExisting,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Data.List.NonEmpty as NonEmpty

import qualified Wilde.Media.Database as DatabaseMedia

import Wilde.ObjectModel.ObjectModel
import Wilde.ObjectModel.UserInteraction
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.Presentation as Presentation
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr

import Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo.Configuration

import qualified Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo.UserInteraction as UserInteractionImpls
import qualified Wilde.ApplicationConstruction.UserInteraction.Io as UiIo
import qualified Wilde.ApplicationConstruction.GenericStringRepIo as GenericStringRepIo


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data EmptyAtAnnotation dbTable e c = EmptyAtAnnotation

-- | Constructor of 'EmptyAtAnnotation'.
mkEmptyAtAnnotation :: EmptyAtAnnotation dbTable e c
mkEmptyAtAnnotation = EmptyAtAnnotation

-- | Type for the Confiuration defined in this module combined
-- with an empty annotation.
type ConfigurationSansAnnotation = Configuration EmptyAtAnnotation


-------------------------------------------------------------------------------
-- - instance -
-------------------------------------------------------------------------------


instance OmGsr.ATTRIBUTE_INPUT_FOR_CREATE (Configuration a) where
  atInputerForCreate = OmGsr.gsrInputer . OmGsr.atIoForCreate

instance OmGsr.ATTRIBUTE_OUTPUT_FOR_CREATE (Configuration a) where
  atOutputerForCreate = OmGsr.gsrOutputer . OmGsr.atIoForCreate

instance OmGsr.ATTRIBUTE_IO_FOR_CREATE (Configuration a) where
  atIoForCreate = GenericStringRepIo.agsrioCreateIo . genericStringRepIo . atConfiguration


instance OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING (Configuration a) where
  atInputerForExisting = OmGsr.gsrInputer . OmGsr.atIoForExisting

instance OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING (Configuration a) where
  atOutputerForExisting = OmGsr.gsrOutputer . OmGsr.atIoForExisting

instance OmGsr.ATTRIBUTE_IO_FOR_EXISTING (Configuration a) where
  atIoForExisting = GenericStringRepIo.agsrioExistingIo . genericStringRepIo . atConfiguration



instance ATTRIBUTE_INPUT_FOR_CREATE (Configuration a) where
  atInputerForCreate = UserInteractionImpls.atInputerForCreate

instance ATTRIBUTE_INPUT_FOR_EXISTING (Configuration a) where
  atInputerForExisting = UserInteractionImpls.atInputerForExisting

instance ATTRIBUTE_OUTPUT_FOR_EXISTING (Configuration a) where
  atOutputerForExisting = UserInteractionImpls.atOutputerForExisting

instance ATTRIBUTE_FOR_CREATE (Configuration a) where
  atCreateOption = UserInteractionImpls.atCreateOption

instance ATTRIBUTE_OUTPUT_FOR_CREATE (Configuration a) where
  atOutputerForCreate = UserInteractionImpls.atOutputerForCreate

instance ATTRIBUTE_IO_FOR_CREATE (Configuration a) where

instance ATTRIBUTE_IO_FOR_EXISTING (Configuration a) where

instance Presentation.ATTRIBUTE_PRESENTATION (Configuration a) where
  atTitle = atpoTitle . presentationO . atConfiguration

instance Database.COLUMN_NAMES (Configuration a) where
  atColumns = eatiDatabaseColumns .
              databaseConfForExisting .
              atConfiguration

instance Database.OUTPUT_FOR_CREATE (Configuration a) where
  atOutputerForCreate = aticDatabaseOutputer .
                        mediaIoForCreate .
                        atConfiguration

instance Database.OUTPUT_FOR_EXISTING (Configuration a) where
  atOutputerExisting = DatabaseMedia.dbOutputer .
                       getDatabaseIoExisting

instance Database.INPUT_FOR_EXISTING (Configuration a) where
  atInputerExisting = DatabaseMedia.dbInputer .
                      getDatabaseIoExisting

instance DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO (Configuration a) where
  atDbPresentationInfoGetter = dbPresentationInfoGetter . atConfiguration

instance Database.IO_FOR_EXISTING (Configuration a) where

instance Database.COLUMNS_AND_IO_FOR_EXISTING (Configuration a) where

instance Database.DATABASE_IO (Configuration a) where


-------------------------------------------------------------------------------
-- - utils -
-------------------------------------------------------------------------------


getDatabaseIoExisting :: AttributeType (Configuration ann) dbTable typeForExisting typeForCreate
                      -> DatabaseMedia.DatabaseIo typeForExisting
getDatabaseIoExisting = atdbioeIo .
                        databaseConfForExisting .
                        atConfiguration

atUiIoForExisting :: AttributeType (Configuration ann) dbTable typeForExisting typeForCreate
                  -> UiIo.AttributeTypeUiIoForExisting typeForExisting
atUiIoForExisting at = uiIoForAttributeName . atConfiguration $ at

eatiDatabaseColumns :: AttributeTypeDatabaseConfigForExisting dbTable typeForExisting
                    -> NonEmpty.NonEmpty (DatabaseMedia.DatabaseColumn dbTable)
eatiDatabaseColumns = atdbioeStructure
