-- | Import this module qualified.

{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Wilde.Render.DataAndButtonsComponent as TopComp


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

mainForObj' :: forall otConf atConf dbTable otNative idAtExisting idAtCreate.
               Presentation.ATTRIBUTE_PRESENTATION atConf
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
  do
    mainComponent <- getMainComponent
    depComponents <- getDepComponents
    pure (theTitle,mainComponent : depComponents)
  where
    getButtons :: Presentation.Monad [AnySVALUE]
    getButtons = do
      id2btnList <- sequence getMkButtonComponents
      pure $ map (\id2btn -> id2btn pk) id2btnList

    getMainComponent :: Presentation.Monad AnyCOMPONENT
    getMainComponent = do
      dataComponent <- showOneComponent atsOrder o
      buttons       <- getButtons
      pure $ TopComp.new dataComponent buttons

    getDepComponents :: Presentation.Monad [AnyCOMPONENT]
    getDepComponents = do
      depComponentMbs <- sequence mkDepComponents
      pure $ catMaybes depComponentMbs
      where
        mkDepComponents = map (\depCompFun -> depCompFun o) depComps

    pk :: idAtExisting
    pk = attrValue $ oIdAttribute o

    ot :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
    ot = oType o
