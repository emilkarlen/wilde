{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectSetup
(
  mkObjectSetup,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.ObjectModelUtils as OmUtils
import qualified Wilde.ObjectModel.Presentation as OmPres

import qualified Wilde.ApplicationConstruction.Presentation.ObjectList as OL


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


mkObjectSetup
  :: forall otConf atConf dbTable otNative idAtExisting idAtCreate.
     OmPres.ATTRIBUTE_PRESENTATION atConf
  => AttributeTypeListSetup.Setup otConf atConf dbTable otNative idAtExisting idAtCreate
  -> OL.ObjectTypeSetup (Object   otConf atConf dbTable otNative idAtExisting idAtCreate)
mkObjectSetup atListSetup =
  OL.ObjectTypeSetup
  {
    OL.otsAttrTitles = map getAtTitle attributeTypes
  , OL.otsGettAttrs  = sequence . mapAttributeAnyValue attrPresentation . objToAttrs
  }
  where
    objToAttrs          :: Object otConf atConf dbTable otNative idAtExisting idAtCreate
                        -> [Any (Attribute atConf dbTable)]
    objToAttrs           = AttributeTypeListSetup.apply atListSetup

    attributeTypes      :: [Any (AttributeType atConf dbTable)]
    attributeTypes       = AttributeTypeListSetup.getAts atListSetup

    getAtTitle :: Any (AttributeType atConf dbTable) -> WildeTitle
    getAtTitle (Any at) = OmPres.atTitle at
