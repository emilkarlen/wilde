-- | Utilities for services that show a list of zero or more 'Object's.
--
-- Import this module qualified.
module Wilde.ApplicationConstruction.StandardServices.ShowManyUtils
       (
         Config(..),
         showMany,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.ApplicationConstruction.UserInteraction.Output.ObjectListSetup as OLS

import qualified Wilde.WildeUi.WildeStyles as WS

import qualified Wilde.Media.Presentation as Presentation

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.Presentation as OmPres

import qualified Wilde.ApplicationConstruction.Presentation.DataAndButtonsComponent as TopComp
import qualified Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectListComponent as OlComp

import           Wilde.Application.ObjectTypeService
import           Wilde.Application.Service.Service

import           Wilde.WildeUi.UiPrimitives


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Configuration
data Config otConf atConf dbTable otNative idAtExisting idAtCreate =
  Config
  {
    title           :: WildeTitle
  , objectListSetup :: OLS.ObjectListSetup otConf atConf dbTable otNative idAtExisting idAtCreate
  }

showMany :: ATTRIBUTE_PRESENTATION atConf
         => ObjectType   otConf atConf dbTable otNative idAtExisting idAtCreate
         -> Config       otConf atConf dbTable otNative idAtExisting idAtCreate
         -> [Object      otConf atConf dbTable otNative idAtExisting idAtCreate]
         -> Service
showMany ot config os =
  do
    x <- toServiceMonad $ showMany' ot config os
    pageOkResult x

showMany' :: ATTRIBUTE_PRESENTATION atConf
          => ObjectType   otConf atConf dbTable otNative idAtExisting idAtCreate
          -> Config       otConf atConf dbTable otNative idAtExisting idAtCreate
          -> [Object      otConf atConf dbTable otNative idAtExisting idAtCreate]
          -> Presentation.Monad (WildeTitle,[AnyCOMPONENT])
showMany' ot (Config title
                (OLS.ObjectListSetup
                 {
                   OLS.displaySetup = OLS.ObjectListDisplaySetup
                                     {
                                       OLS.displayAts  = theDisplayAts
                                     , OLS.getMkFooterRowsConstructor = theGetFooterRowsConstructor
                                     }
                 , OLS.buttonsSetup = OLS.ObjectListButtonsSetup
                                      {
                                        OLS.objectButtonsLeft      = objBtnsLeft
                                      , OLS.objectButtonsRight     = objBtnsRight
                                      , OLS.objectTypeButtonsBelow = getObjTypeBtnsBelow
                                      }
                 }))
  os =
    do
      atListSetup             <- Presentation.toPresentationMonad $
                                 AttributeTypeListSetup.mkGeneral ot theDisplayAts
      mkFooterRowsConstructor <- theGetFooterRowsConstructor
      objectListComponent     <- OlComp.objectList
                                 componentStyle
                                 Nothing
                                 atListSetup
                                 mkFooterRowsConstructor
                                 objBtnsLeft objBtnsRight
                                 (pure os)
      buttonsBelow            <- sequence getObjTypeBtnsBelow
      pure (title, [TopComp.new objectListComponent buttonsBelow])

    where
      componentStyle :: WildeStyle
      componentStyle  = WS.presentationTableMulti <> OmPres.objectTypeStyle ot
