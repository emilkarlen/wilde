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


import qualified Data.List.NonEmpty as NonEmpty

import           Wilde.WildeUi.UiPrimitives (WildeTitle)

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

import           Wilde.Application.Service.Service


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

  , updatables :: NonEmpty.NonEmpty (Any (AttributeType atConf dbTable))
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
                                 pure $ FormBlocksAndMetas [] [formBlock]
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

    firstPageTitle :: WildeTitle
    firstPageTitle = ServiceUtils.page1Title titles

    resultPageTitle :: WildeTitle
    resultPageTitle = ServiceUtils.page2Title titles

-- | Gives the updatable 'AttributeType's of a configuration.
updatableAts :: [OutputForExisting.AttributeTypeConfiguration atConf dbTable]
             -> Maybe (NonEmpty.NonEmpty (Any (AttributeType atConf dbTable)))
updatableAts = NonEmpty.nonEmpty . updatableAtsList

updatableAtsList :: [OutputForExisting.AttributeTypeConfiguration atConf dbTable]
                 -> [Any (AttributeType atConf dbTable)]
updatableAtsList = OutputForExisting.atsWith OutputForExisting.UserInteraction


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
