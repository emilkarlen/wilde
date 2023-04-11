{-# LANGUAGE KindSignatures #-}

-------------------------------------------------------------------------------
-- | The standard service \"Create One\".
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.StandardServices.CreateOne
       (

         Config(..),
         mkService,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation
import qualified Wilde.Media.UserInteraction.Output as UiO
import qualified Wilde.Media.UserInteraction as UI
import qualified Wilde.ObjectModel.UserInteraction.Output.ForCreateFrom as OutputForCreateFrom
import qualified Wilde.ObjectModel.UserInteraction.Output.ForCreate as OutputForCreate
import qualified Wilde.ObjectModel.UserInteraction.Input.ForCreate as InputForCreate
import qualified Wilde.ObjectModel.Presentation as Presentation

import qualified Wilde.ApplicationConstruction.Service.ServiceUtils as ServiceUtils
import           Wilde.ApplicationConstruction.Service.StepService
import           Wilde.Application.ObjectTypeService

import Wilde.ApplicationConstruction.StandardServices.CreateOneUtils
    ( inputFromUi_store_show )
import           Wilde.ApplicationConstruction.StandardServices.SingleObjectServiceCommon

import           Wilde.WildeUi.WildeStyle (WildeStyle)


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Config (otConf :: * -> * -> * -> * -> *) atConf dbTable otNative idAtExisting idAtCreate =
  Config
  {
    titles :: ServiceUtils.TwoStepServiceTitles
    -- | Specifies the order in which
    -- the 'AttributeType's inputers are listed.
    -- The list must be a permutation of all 'AttributeType's of the
    -- 'ObjectType'.
  , attributeTypesOrder :: [Any (AttributeType atConf dbTable)]
  }

mkService :: (Database.OBJECT_TYPE_INSERT otConf
             ,Database.DATABASE_IO atConf
             ,Presentation.ATTRIBUTE_PRESENTATION atConf
             ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
             ,InputForCreate.ATTRIBUTE_INPUT_FOR_CREATE atConf
             ,OutputForCreateFrom.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
             )
          => [AnyO (OtServiceOtSetup Config otConf atConf)] -> AnyOtService
mkService otss = AnyOtService $
  OtService
  {
    main  = AnyObjectTypeServiceMainFunction createOneMain
  , types = otss
  }

createOneMain :: (Database.OBJECT_TYPE_INSERT otConf
                 ,Database.DATABASE_IO atConf
                 ,Presentation.ATTRIBUTE_PRESENTATION atConf
                 ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
                 ,InputForCreate.ATTRIBUTE_INPUT_FOR_CREATE atConf
                 ,OutputForCreateFrom.ATTRIBUTE_OUTPUT_FOR_CREATE atConf
                 )
              => ObjectTypeServiceMainFunction Config otConf atConf dbTable otNative idAtExisting idAtCreate
createOneMain ot (Config titles attributeTypesOrder) = stepService def
  where
    def :: StepService
    def = StepService
          {
            mainTitle    = ServiceUtils.page1Title titles
          , nonLastSteps = [outputForm]
          , lastStep     = inputFromUi_store_show ot
                           (ServiceUtils.page2Title titles)
                           attributeTypesOrder theObjectName
          }

    style      :: WildeStyle
    style       = Presentation.objectTypeStyle ot

    outputForm :: NonLastStep
    outputForm = toServiceMonad outputForm' >>= continue

    outputForm' :: UiO.Monad FormBlocksAndMetas
    outputForm' =
      do
        getFormBlock' <- OutputForCreate.outputerForStdSetup attributeTypesOrder
        let formBlock' = getFormBlock' theObjectName
        let formBlock = UI.formBlock_appendStyle style formBlock'
        pure $ FormBlocksAndMetas [] [formBlock]
