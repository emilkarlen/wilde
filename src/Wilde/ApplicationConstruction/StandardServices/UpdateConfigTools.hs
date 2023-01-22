-------------------------------------------------------------------------------
-- | Utilities for setting the Update Configuration of
-- 'StandardServices.ObjectTypeSetup'.
--
-- Usage: import qualified
-------------------------------------------------------------------------------
module Wilde.ApplicationConstruction.StandardServices.UpdateConfigTools
       (
         Setter,
         withConfigAs,
         updateNotPossible,
         allUpdatable,
         displayIdAndUpdateOthers,
         displayIdAndUpdateOthers_usingStdAttributeOrder,
         sortAccordingToStdAttributeTypeOrder,
         thisConfig,
       )
       where

-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.Utils.ListUtils as ListUtils

import Wilde.ObjectModel.ObjectModel

import qualified Wilde.ApplicationConstruction.StandardServices as StandardServices
import qualified Wilde.ApplicationConstruction.StandardServices.UpdateOne as UpdateOne


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


type Setter otConf atConf dbTable otNative idAtExisting idAtCreate =
  StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
  -> UpdateOne.AttributeTypeConfigurations atConf dbTable

withConfigAs :: StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
             -> Setter                           otConf atConf dbTable otNative idAtExisting idAtCreate
             -> StandardServices.ObjectTypeSetup otConf atConf dbTable otNative idAtExisting idAtCreate
withConfigAs ots f
  =
  ots
  {
    StandardServices.updateConfig = f ots
  }

-- | Update is not supported by the setup.
updateNotPossible :: Setter otConf atConf dbTable otNative idAtExisting idAtCreate
updateNotPossible = thisConfig []

-------------------------------------------------------------------------------
-- | All 'AttributeType's are updatable.
--
-- Order is that the \"internal\" order - the order the types are listed
-- in the 'ObjectType'.
-------------------------------------------------------------------------------
allUpdatable :: Setter otConf atConf dbTable otNative idAtExisting idAtCreate
allUpdatable (StandardServices.ObjectTypeSetup{
                 StandardServices.objectType = theOt
                 })
  =
  map UpdateOne.mkUpdatable (otAttributeTypes theOt)

-------------------------------------------------------------------------------
-- | Displays the ID attribute, but makes it impossible to update.
-- All other attributes are updatable.
--
-- Precondition: The 'ObjectType' must have at least one non-ID attribute.
-------------------------------------------------------------------------------
displayIdAndUpdateOthers :: Setter otConf atConf dbTable otNative idAtExisting idAtCreate
displayIdAndUpdateOthers (StandardServices.ObjectTypeSetup{
                             StandardServices.objectType = theOt })
  =
  UpdateOne.mkDisplayed (Any (otIdAttributeType theOt)) :
  map UpdateOne.mkUpdatable (otNonIdAttributeTypes theOt)

-------------------------------------------------------------------------------
-- | As 'displayIdAndUpdateOthers', but the attributes are ordered using the
-- existing order-spec in the setup.
-------------------------------------------------------------------------------
displayIdAndUpdateOthers_usingStdAttributeOrder :: Setter otConf atConf dbTable otNative idAtExisting idAtCreate
displayIdAndUpdateOthers_usingStdAttributeOrder setup
  = sortAccordingToStdAttributeTypeOrder $
    setup `withConfigAs` displayIdAndUpdateOthers

-------------------------------------------------------------------------------
-- | Sorts the 'AttributeType's in the config according to the \"standard\"
-- order specified in the setup.
-------------------------------------------------------------------------------
sortAccordingToStdAttributeTypeOrder :: Setter otConf atConf dbTable otNative idAtExisting idAtCreate
sortAccordingToStdAttributeTypeOrder
  setup@(StandardServices.ObjectTypeSetup {
            StandardServices.alternativeAtsOrder = theAltOrder
            })
  = ListUtils.orderAccordingToList
    getKeyFromSpecificationElem
    getKeyFromUnorderedElem
    (StandardServices.singleObjectAttributeTypeOrder setup)
    (displayIdAndUpdateOthers setup)
    where
      getKeyFromSpecificationElem (Any (AttributeType {atCrossRefKey = key})) = key
      getKeyFromUnorderedElem = getKeyFromSpecificationElem . UpdateOne.configAt


-------------------------------------------------------------------------------
-- | A given constant configuration.
-------------------------------------------------------------------------------
thisConfig :: UpdateOne.AttributeTypeConfigurations atConf dbTable
           -> Setter otConf atConf dbTable otNative idAtExisting idAtCreate
thisConfig = const
