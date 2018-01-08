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

-------------------------------------------------------------------------------
-- | Components for use by an application.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.UserInteraction.Output.SpecialComponents
       (
         objectListTableAccordingToSetup,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.Media.WildeValue
import qualified Wilde.Media.Presentation as Presentation

import           Wilde.WildeUi.WildeComponent
import qualified Wilde.WildeUi.LayoutValues      as LayoutValues

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.Presentation


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


objectListTableAccordingToSetup :: ATTRIBUTE_PRESENTATION atConf
                                => WildeStyle
                                -> AttributeTypeListSetup.Setup     otConf atConf dbTable otNative idAtExisting idAtCreate
                                -> Maybe (FooterRowsConstructor acc otConf atConf dbTable otNative idAtExisting idAtCreate)
                                -> [Presentation.Monad (idAtExisting -> AnySVALUE)]
                                -> [Presentation.Monad (idAtExisting -> AnySVALUE)]
                                -> Maybe StyledTitle
                                -> [Object                          otConf atConf dbTable otNative idAtExisting idAtCreate]
                                -> Presentation.Monad AnyCOMPONENT
objectListTableAccordingToSetup tableStyle
  atListSetup
  mbFooterRowsConstructor
  getLeftSideContentConstructors
  getRightSideContentConstructors
  mbTitle
  os =
    do
      leftSideContentConstructors    <- sequence getLeftSideContentConstructors
      rightSideContentConstructors   <- sequence getRightSideContentConstructors
      let leftSideContentConstructor  = mkSideContent leftSideContentConstructors
      let rightSideContentConstructor = mkSideContent rightSideContentConstructors
      let mkGenericTable = objectListTable
                           atListSetup mbFooterRowsConstructor
                           leftSideContentConstructor
                           rightSideContentConstructor
      genericTable      <- mkGenericTable mbTitle os
      let styledTable    = addStyleToSTYLING tableStyle genericTable
      return $ AnyCOMPONENT $ TableListComponent Nothing styledTable
  where
    mkSideContent mkButtons pk  = if null mkButtons
                                  then empty
                                  else LayoutValues.horizontal [mkBtn pk | mkBtn <- mkButtons]
