-- | Import this module qualified.
module Wilde.ApplicationConstruction.StandardServices.ShowOne
       (
         mkService,

         showOnePage,
         showOnePageService,

         Config(..),
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import Data.Maybe

import           Wilde.Media.WildeValue (AnySVALUE)
import qualified Wilde.Media.Presentation as Presentation

import qualified Wilde.WildeUi.LayoutValues as LayoutValues
import qualified Wilde.WildeUi.LayoutComponents  as LayoutComponents

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.Presentation as Presentation
import qualified Wilde.ObjectModel.Database as Database
import qualified Wilde.ObjectModel.DatabaseAndPresentation as DatabaseAndPresentation
import qualified Wilde.ObjectModel.GenericStringRep as OmGsr

import Wilde.Application.Service.Service
import Wilde.Application.ObjectTypeService

import Wilde.ApplicationConstruction.UserInteraction.Output.ObjectDependentComponent
import Wilde.ApplicationConstruction.Service.ObjectTypeServiceUtils
import Wilde.ApplicationConstruction.Service.ServiceUtils


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


data Config otConf atConf dbTable otNative idAtExisting idAtCreate =
  Config
  {
    title               :: StyledTitle
  , attributeTypesOrder :: [Any (AttributeType  atConf dbTable)]
  , dependentComponents :: [ObjectDependentComponent   otConf atConf dbTable otNative idAtExisting idAtCreate]
  , buttons             :: [Presentation.Monad (idAtExisting -> AnySVALUE)]
  }

mkService :: (Database.DATABASE_TABLE otConf
             ,Presentation.ATTRIBUTE_PRESENTATION atConf
             ,Database.COLUMNS_AND_IO_FOR_EXISTING atConf
             ,DatabaseAndPresentation.ATTRIBUTE_TYPE_INFO atConf
             ,OmGsr.ATTRIBUTE_INPUT_FOR_EXISTING atConf
             )
          => [AnyO (OtServiceOtSetup Config otConf atConf)]
          -> AnyOtService
mkService otss = AnyOtService $
  OtService
  {
    main  = AnyObjectTypeServiceMainFunction $ objectServiceMainFunction mainForObj
  , types = otss
  }

mainForObj :: Presentation.ATTRIBUTE_PRESENTATION atConf
           => ObjectServiceMainFunction Config otConf atConf dbTable otNative idAtExisting idAtCreate
mainForObj o config =
   do
      titleAndComponents <- toServiceMonad $ mainForObj' o config
      pageOkResult titleAndComponents

mainForObj' :: Presentation.ATTRIBUTE_PRESENTATION atConf
            => Object otConf atConf dbTable otNative idAtExisting idAtCreate
            -> Config otConf atConf dbTable otNative idAtExisting idAtCreate
            -> Presentation.Monad (StyledTitle,[AnyCOMPONENT])
mainForObj' o (Config
               {
                 title               = theTitle
               , attributeTypesOrder = atsOrder
               , dependentComponents = depComps
               , buttons             = getMkButtonComponents
               }) =
  let
    ot               = oType o
    pk               = attrValue $ oIdAttribute o
    mkDepComponents  = map (\depCompFun -> depCompFun o) depComps
  in
   do
     mkButtonComponents           <- sequence getMkButtonComponents
     let buttonComponents          = map (\mk -> mk pk) mkButtonComponents
     let buttonsComponentList      = if null buttonComponents
                                     then []
                                     else [LayoutComponents.svalueComponent $
                                           LayoutValues.horizontal
                                           buttonComponents] :: [AnyCOMPONENT]
     (title,showOneComponents)    <- showOnePage atsOrder theTitle o
     let mainComponents            = showOneComponents ++ buttonsComponentList
     let mainComponent             = LayoutComponents.verticalComponents mainComponents
     depComponentMbs <- sequence mkDepComponents
     let depComponents = catMaybes depComponentMbs
     pure (title,mainComponent : depComponents)
