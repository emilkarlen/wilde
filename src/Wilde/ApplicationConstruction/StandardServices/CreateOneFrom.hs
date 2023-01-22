{-# LANGUAGE KindSignatures #-}

-------------------------------------------------------------------------------
-- | The standard service \"Create One From\".
--
-- This service creates an 'Object' populating the input form with
-- default values from a given existing 'Object'.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.StandardServices.CreateOneFrom
       (
         Config(..),
         mkService,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Media.UserInteraction.Io

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation
import qualified Wilde.ObjectModel.UserInteraction.Output.ForCreateFrom as OutputForCreateFrom
import qualified Wilde.ObjectModel.UserInteraction.Input.ForCreate as InputForCreate
import qualified Wilde.ObjectModel.Presentation as Presentation
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr

import Wilde.Application.ObjectTypeService

import qualified Wilde.ApplicationConstruction.Service.ServiceUtils as ServiceUtils
import           Wilde.ApplicationConstruction.Service.StepService
import qualified Wilde.ApplicationConstruction.Service.ServiceTools as ServiceTools

import Wilde.ApplicationConstruction.StandardServices.CreateOneUtils
import Wilde.ApplicationConstruction.StandardServices.SingleObjectServiceCommon

-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Config (otConf :: * -> * -> * -> * -> *) atConf dbTable otNative idAtExisting idAtCreate =
  Config
  {
    titles              :: ServiceUtils.TwoStepServiceTitles
    -- | Specifies the order in which
    -- the 'AttributeType's inputers are listed.
    -- The list must be a permutation of all 'AttributeType's of the
    -- 'ObjectType'.
  , attributeTypesOrder :: [Any (AttributeType atConf dbTable)]
  }

mkService :: (Database.OBJECT_TYPE_INSERT otConf
             ,Presentation.ATTRIBUTE_PRESENTATION atConf
             ,Database.DATABASE_IO atConf
             ,Database.COLUMN_NAMES atConf
             ,InputForCreate.ATTRIBUTE_INPUT_FOR_CREATE atConf
             ,OutputForCreateFrom.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
             ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
             ,OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
             )
          => [AnyO (OtServiceOtSetup Config otConf atConf)] -> AnyOtService
mkService otss = AnyOtService $
  OtService
  {
    main  = AnyObjectTypeServiceMainFunction createOneFromMain
  , types = otss
  }

createOneFromMain :: (Database.OBJECT_TYPE_INSERT otConf
                     ,Presentation.ATTRIBUTE_PRESENTATION atConf
                     ,Database.DATABASE_IO atConf
                     ,InputForCreate.ATTRIBUTE_INPUT_FOR_CREATE atConf
                     ,OutputForCreateFrom.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
                     ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                     ,OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
                     )
                  => ObjectTypeServiceMainFunction Config otConf atConf dbTable otNative idAtExisting idAtCreateFrom
createOneFromMain ot (Config titles attributeTypesOrder) = stepService def
  where
    def :: StepService
    def = StepService
          {
            mainTitle    = ServiceUtils.page1Title titles
          , nonLastSteps = [inputFromDb_and_outputForm]
          , lastStep     = inputFromUi_store_show ot
                           (ServiceUtils.page2Title titles)
                           attributeTypesOrder theObjectName
          }

    inputFromDb_and_outputForm :: NonLastStep
    inputFromDb_and_outputForm =
      do
        formBlockAndMetas <- ServiceTools.withObjectFromDbWithIdFromEnv ot
                             (\o -> do
                                 formBlock <- toServiceMonad $
                                              mkFormBlock
                                              attributeTypesOrder
                                              o
                                              theObjectName
                                 return $ FormBlocksAndMetas [] [formBlock]
                             )
        continue formBlockAndMetas

-------------------------------------------------------------------------------
-- | Gives a 'FormBlock' with widgets for all values that should be input
-- via User Interaction for creating an 'Object' from an existing 'Object'.
-------------------------------------------------------------------------------
mkFormBlock :: OutputForCreateFrom.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
            => [Any (AttributeType atConf dbTable)]
            -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
            -> ObjectName
            -> UserInteractionOutputMonad FormBlock
mkFormBlock attributeTypesOrder o oName =
  do
    formBlockForObjectName <- OutputForCreateFrom.outputer attributeTypesOrder o
    return $ formBlockForObjectName oName
