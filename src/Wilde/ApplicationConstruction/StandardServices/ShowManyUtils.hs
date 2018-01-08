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

import qualified Wilde.Media.WildeStyle as WS

import qualified Wilde.WildeUi.LayoutValues as LayoutValues
import qualified Wilde.WildeUi.LayoutComponents as LayoutComponents

import qualified Wilde.Media.Presentation as Presentation

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.Presentation

import Wilde.ApplicationConstruction.UserInteraction.Output.SpecialComponents
import Wilde.Application.ObjectTypeService


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Configuration
data Config otConf atConf dbTable otNative idAtExisting idAtCreate =
  Config
  {
    title           :: StyledTitle
  , objectListSetup :: (OLS.ObjectListSetup otConf atConf dbTable otNative idAtExisting idAtCreate)
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
          -> Presentation.Monad (StyledTitle,[AnyCOMPONENT])
showMany' ot (Config title
                (OLS.ObjectListSetup
                 {
                   OLS.displaySetup = OLS.ObjectListDisplaySetup
                                     {
                                       OLS.displayAts  = theDisplayAts
                                     , OLS.orderByInDb = theOrderByInDb
                                     , OLS.getFooterRowsConstructor = theGetFooterRowsConstructor
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
      mbFooterRowsConstructor <- theGetFooterRowsConstructor
      tableComponent          <- objectListTableAccordingToSetup
                                 WS.presentationTableMulti
                                 atListSetup
                                 mbFooterRowsConstructor
                                 objBtnsLeft objBtnsRight
                                 Nothing
                                 os
      objTypeBtnsBelow <- sequence getObjTypeBtnsBelow
      let mbButtonsComponent = if null objTypeBtnsBelow
                               then Nothing
                               else Just $ LayoutComponents.svalueComponent $
                                    LayoutValues.horizontal
                                    objTypeBtnsBelow
      return (title,
              tableComponent : maybe [] (:[]) mbButtonsComponent)
