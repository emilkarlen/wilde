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

{-# LANGUAGE KindSignatures #-}

-------------------------------------------------------------------------------
-- | The standard service \"Update One\".
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.StandardServices.UpdateOne
       (
         Config(..),
         
         OutputForExisting.AttributeTypeRole(..),
         OutputForExisting.AttributeTypeConfiguration(..),
         OutputForExisting.AttributeTypeConfigurations(..),
         
         mkUpdatable,
         mkDisplayed,

         mkService,
         
         -- * Utilities
         
         updatableAts,
         updatableAtsList, -- tryging to avoid infinite loop
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.NonEmptyList as NonEmpty

import qualified Wilde.Media.MonadWithInputMedia as MIIA
import           Wilde.Media.UserInteraction.Io
import           Wilde.Media.UserInteraction.Output as UIO

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr
import qualified Wilde.ObjectModel.Database as Database

import qualified Wilde.ObjectModel.UserInteraction as UserInteraction
import qualified Wilde.ObjectModel.UserInteraction.Output.ForExisting as OutputForExisting
import qualified Wilde.ObjectModel.Presentation as Presentation
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation

import qualified Wilde.ApplicationConstruction.Service.ServiceUtils as ServiceUtils
import           Wilde.ApplicationConstruction.Service.StepService
import qualified Wilde.ApplicationConstruction.Service.ServiceTools as ServiceTools

import           Wilde.ApplicationConstruction.StandardServices.SingleObjectServiceCommon

import qualified Wilde.ApplicationConstruction.ElementSetUtils as ESU
import qualified Wilde.ApplicationConstruction.UserInteraction.Io as UiIo

import           Wilde.Application.ObjectTypeService


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Config (otConf :: * -> * -> * -> * -> *) atConf dbTable otNative idAtExisting idAtCreate =
  Config
  {
    titles              :: ServiceUtils.TwoStepServiceTitles
    
    -- | The display order of the attributes when presenting
    -- the updated object.
    -- This must be a permutation of all 'AttributeType's of the
    -- 'ObjectType'.
  , displayUpdatedObjectConfig :: [Any (AttributeType atConf dbTable)]
    
    -- | Configures the input form.
    --
    -- Must contain at least one updatable attribute type.
  , inputFormConfig :: OutputForExisting.AttributeTypeConfigurations atConf dbTable

  , updatables :: NonEmpty.List (Any (AttributeType atConf dbTable))
  }
      
mkService :: (Database.DATABASE_TABLE otConf
             ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
             ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
             ,Presentation.ATTRIBUTE_PRESENTATION atConf
             ,UserInteraction.ATTRIBUTE_IO_FOR_EXISTING atConf
             ,OmGsr.ATTRIBUTE_IO_FOR_EXISTING atConf
             )
          => [AnyO (OtServiceOtSetup Config otConf atConf)] 
          -> AnyOtService
mkService otss = AnyOtService $
  OtService
  {
    main  = AnyObjectTypeServiceMainFunction updateOneMain
  , types = otss
  }

updateOneMain :: (Database.DATABASE_TABLE otConf
                 ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
                 ,OmGsr.ATTRIBUTE_IO_FOR_EXISTING atConf
                 ,Presentation.ATTRIBUTE_PRESENTATION atConf
                 ,UserInteraction.ATTRIBUTE_IO_FOR_EXISTING atConf
                 ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                 )
              => ObjectTypeServiceMainFunction Config otConf atConf dbTable otNative idAtExisting idAtUpdate
updateOneMain ot (Config titles displayUpdatedObjectConfig inputFormConfig updatables) =
  stepService def
  where
    def :: StepService
    def = StepService
          {
            mainTitle    = firstPageTitle
          , nonLastSteps = [inputFromDb_and_mkInputForm]
          , lastStep     = inputFromUi_store_show
          }

    inputFromDb_and_mkInputForm :: NonLastStep
    inputFromDb_and_mkInputForm =
      do
        formBlockAndMetas <- ServiceTools.withObjectFromDbWithIdFromEnv ot
                             (\o -> do
                                 formBlock'   <- toServiceMonad $
                                                 objectFormBlock 
                                                 inputFormConfig
                                                 o 
                                                 theObjectName
                                 let formBlock = outputOriginalObjectId o formBlock'
                                 return $ FormBlocksAndMetas [] [formBlock]
                               )
        continue formBlockAndMetas

    inputFromUi_store_show :: Service
    inputFromUi_store_show =
      do
        objId <- inputOriginalObjectId ot
        o     <- (ServiceUtils.updateObject ot updatables (theObjectName,objId) >>= 
                  ServiceTools.swallowError)
        page <- ServiceUtils.showOnePageService displayUpdatedObjectConfig resultPageTitle o
        pageOkResult page

    firstPageTitle :: StyledTitle
    firstPageTitle = ServiceUtils.page1Title titles

    resultPageTitle :: StyledTitle
    resultPageTitle = ServiceUtils.page2Title titles

-- | Gives the updatable 'AttributeType's of a configuration.
updatableAts :: [OutputForExisting.AttributeTypeConfiguration atConf dbTable]
             -> Maybe (NonEmpty.List (Any (AttributeType atConf dbTable)))
updatableAts = NonEmpty.fromList . updatableAtsList

updatableAtsList :: [OutputForExisting.AttributeTypeConfiguration atConf dbTable]
                 -> [Any (AttributeType atConf dbTable)]
updatableAtsList = OutputForExisting.atsWith OutputForExisting.UserInteraction
    

-- updatableAts_debug_orig :: OutputForExisting.AttributeTypeConfigurations atConf dbTable
--              -> Maybe (NonEmpty.List (Any (AttributeType atConf dbTable)))
-- updatableAts_debug_orig = NonEmpty.fromList . 
--                OutputForExisting.atsWith OutputForExisting.UserInteraction

-- | Configuration for an updatable 'AttributeType'.
mkUpdatable :: (Any (AttributeType atConf dbTable))
            -> OutputForExisting.AttributeTypeConfiguration atConf dbTable
mkUpdatable at =
  OutputForExisting.AttributeTypeConfiguration
  {
    OutputForExisting.configRole = OutputForExisting.UserInteraction
  , OutputForExisting.configAt   = at
  }

-- | Configuration for an 'AttributeType' that is not updatable,
-- but displayed in the input form.
mkDisplayed :: (Any (AttributeType atConf dbTable))
            -> OutputForExisting.AttributeTypeConfiguration atConf dbTable
mkDisplayed at =
  OutputForExisting.AttributeTypeConfiguration
  {
    OutputForExisting.configRole = OutputForExisting.Presentation
  , OutputForExisting.configAt   = at
  }

varNamePrimaryKey :: String
varNamePrimaryKey = "_pk"


-- | Saves the ID of the object in the form block, so that it can later
-- be inputed by 'inputOriginalObjectId'
outputOriginalObjectId :: OmGsr.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                       => Object otConf atConf dbTable otNative idAtExisting idAtCreate
                       -> FormBlock
                       -> FormBlock
outputOriginalObjectId o fb = formBlockAppendMetaValues fb [idElement]
  where
    idElement = element
                ekPrimaryKey
                (OmGsr.objOutputForIdAt o)

-- | Inputs the object ID that was saved by 'outputOriginalObjectId'.
inputOriginalObjectId :: OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
                      => ObjectType otConf atConf dbTable otNative idAtE idAtC
                      -> ServiceMonad idAtE
inputOriginalObjectId ot =
    let
      gsrInputer          = OmGsr.otInputerForIdAtForExisting ot
      parseIdAtExisting s = either (const Nothing) Just $ gsrInputer s
      lookuper            = ESU.gsr_lookuper gsrInputer
    in
     MIIA.inInputMedia (lookuper ekPrimaryKey)

ekPrimaryKey :: ElementKey
ekPrimaryKey = elementKey theObjectName varNamePrimaryKey


-------------------------------------------------------------------------------
-- - AttributeTypePresentationOrUserInteractionInfo -
-------------------------------------------------------------------------------


-- | Information about an 'Attribute' that is to be, either
-- presented or input.
type AttributeTypePresentationOrUserInteractionInfo typeForExisting =
  Either
  (AttributeTypePresentation    typeForExisting)
  (UiIo.AttributeTypeUiIoForExisting typeForExisting)


-------------------------------------------------------------------------------
-- - objectFormBlock -
-------------------------------------------------------------------------------


-- |  Gives a 'FormBlock' with widgets and presentations values for
-- updating an 'Object'.
objectFormBlock :: OutputForExisting.ATTRIBUTE_OUTPUT_FOR_EXISTING atConf
                => OutputForExisting.AttributeTypeConfigurations atConf dbTable
                -> Object otConf atConf dbTable oNative idAtExisting idAtCreate
                -> ObjectName
                -> UserInteractionOutputMonad FormBlock
objectFormBlock atConfigs o oName =
  do
    outputer <- OutputForExisting.outputerObj (oType o) atConfigs
    toUserInteractionOutputMonad $ outputer (oName,o)
