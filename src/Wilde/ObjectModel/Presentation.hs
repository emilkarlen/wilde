-- | Presentation of Object's and 'Attribute's.

{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ObjectModel.Presentation
(
  module F,
  ATTRIBUTE_PRESENTATION(..),
  getAttributesInGivenOrder,

  objectTypeStyle,
  objectTypeStyle_o,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.WildeUi.UiPrimitives

import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.ObjectModelUtils as OmUtils
import           Wilde.ObjectModel.Presentation.FooterRowsConstructor as F


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


class ATTRIBUTE_PRESENTATION atConf where
  atTitle :: AttributeType atConf dbTable typeForExisting typeForCreate
          -> WildeTitle


-------------------------------------------------------------------------------
-- Gives the 'Attribute's in the given order.
-------------------------------------------------------------------------------


getAttributesInGivenOrder :: [Any (AttributeType atConf dbTable)]
                          -- ^ Specifies the order in which
                          -- the 'Attribute's inputers are listed.
                          -> Object otConf atConf dbTable otNative idAtExisting idAtCreate
                          -> GeneralResult [Any (Attribute atConf dbTable)]
getAttributesInGivenOrder attributeTypesOrder o =
  do
    atListSetup <- AttributeTypeListSetup.mkGeneral ot attributeTypesOrder
    pure $ AttributeTypeListSetup.apply atListSetup o
  where
    ot = oType o


-- | Utility to get a WildeStyle of the OT's cross-ref-id.
--
-- Is this the right place
objectTypeStyle
  :: ObjectType otConf atConf dbTable otNative idAtExisting idAtCreate
  -> WildeStyle
objectTypeStyle = WildeStyle . (:[]) . otCrossRefKey

-- | Utility to get a WildeStyle of the O->OT's cross-ref-id.
--
-- Is this the right place
objectTypeStyle_o
  :: Object otConf atConf dbTable otNative idAtExisting idAtCreate
  -> WildeStyle
objectTypeStyle_o = objectTypeStyle . oType
