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

import           Wilde.ObjectModel.ObjectModel
import qualified Wilde.ObjectModel.UserInteraction as OmUi
import qualified Wilde.ObjectModel.UserInteraction.Input.ForExisting as InputForExisting
import qualified Wilde.ObjectModel.UserInteraction.Output.ForExisting as OutputForExisting
import qualified Wilde.ObjectModel.UserInteraction.Output.CreateCommon as OutputCreateCommon

import           Wilde.ApplicationConstruction.AttributeTypeConfiguration.UiIoAndDbIo.Configuration


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
