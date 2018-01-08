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
-- | Helpers for UiIoAndDbIo.UserInteraction: instantiation of classes
-- related to User Interaction.
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo.UserInteraction
       (
         atOutputerForExisting,
         atOutputerForCreate,
         atCreateOption,
         atInputerForCreate,
         atInputerForExisting,
       )
       where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Media.ElementSet as ES
import qualified Wilde.Media.UserInteraction.Input as UiI
import qualified Wilde.Media.UserInteraction.Io as UiIo

import Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.UserInteraction as OmUi
import qualified Wilde.ObjectModel.UserInteraction.Input.ForExisting as InputForExisting
import qualified Wilde.ObjectModel.UserInteraction.Output.ForExisting as OutputForExisting
import qualified Wilde.ObjectModel.UserInteraction.Output.CreateCommon as OutputCreateCommon

import Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo.Configuration


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- - InputForCreate -
-------------------------------------------------------------------------------


atInputerForCreate :: AttributeType (Configuration a)  dbTable typeForExisting typeForCreate
                   -> AttributeName
                   -> UiI.UserInteractionInputer (ES.ElementInputResult typeForCreate)
atInputerForCreate (AttributeType {
                       atConfiguration = Configuration {
                          mediaIoForCreate = theMediaIoForCreate } })
  =
    (UiIo.uiInputer . aticUiIo) theMediaIoForCreate


-------------------------------------------------------------------------------
-- - InputForExisting -
-------------------------------------------------------------------------------


atInputerForExisting :: AttributeType (Configuration a) dbTable typeForExisting typeForCreate
                     -> InputForExisting.AttributeInputer typeForExisting
atInputerForExisting (AttributeType {
                       atConfiguration = Configuration {
                          uiIoForAttributeName = theUiIoForAttributeName } })
  =
  UiIo.uiInputer theUiIoForAttributeName


-------------------------------------------------------------------------------
-- - OutputForExisting -
-------------------------------------------------------------------------------

atOutputerForExisting :: AttributeType 
                         (Configuration ann) 
                         dbTable 
                         typeForExisting typeForCreate
                      -> OutputForExisting.UserInteractionOutputerForExisting
                         typeForExisting
atOutputerForExisting
  (AttributeType { 
      atConfiguration = Configuration { 
         uiIoForAttributeName = theUiIoForAttributeName } }) 
  =
  UiIo.uiOutputer theUiIoForAttributeName
  

-------------------------------------------------------------------------------
-- - OutputCreate -
-------------------------------------------------------------------------------


atOutputerForCreate :: AttributeType (Configuration ann) dbTable typeForExisting typeForCreate 
                    -> AttributeName
                    -> OutputCreateCommon.AttributeTypeOutputerForCreate typeForExisting typeForCreate
atOutputerForCreate (AttributeType { 
                        atConfiguration = Configuration { 
                           mediaIoForCreate = theMediaIoForCreate } })
  =
  (UiIo.uiOutputer . aticUiIo) theMediaIoForCreate
  
atCreateOption :: AttributeType (Configuration ann) dbTable typeForExisting typeForCreate 
               -> Maybe (OmUi.AttributeTypeCreateOption typeForCreate)
atCreateOption (AttributeType { 
                   atConfiguration = Configuration { 
                      uiCreateOption = theCreateOption } })
  = theCreateOption
