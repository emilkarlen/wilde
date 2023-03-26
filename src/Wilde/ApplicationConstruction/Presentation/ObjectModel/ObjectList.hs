-- | Functionality for presenting a list of Wilde objects

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectList
(
  objectList,
)
where


-------------------------------------------------------------------------------
-- - import -
-------------------------------------------------------------------------------


import           Wilde.WildeUi.StdValueTypes as SVT

import qualified Wilde.Media.Presentation as Presentation

import qualified Wilde.ObjectModel.AttributeTypeListSetup.SansAnnotation as AttributeTypeListSetup
import           Wilde.ObjectModel.ObjectModelUtils as OmUtils
import           Wilde.ObjectModel.Presentation (ATTRIBUTE_PRESENTATION(..))
import           Wilde.ObjectModel.Presentation.FooterRowsConstructor2
                   ( FooterConstructor )

import qualified Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectSetup as OS
import qualified Wilde.ApplicationConstruction.Presentation.ObjectList as OL


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


-- | Produces a table that shows a list of 'Object's.
objectList
  :: forall otConf atConf dbTable otNative idAtExisting idAtCreate.
     ATTRIBUTE_PRESENTATION atConf
  => Maybe WildeTitle
  -> AttributeTypeListSetup.Setup     otConf atConf dbTable otNative idAtExisting idAtCreate
  -> FooterConstructor                otConf atConf dbTable otNative idAtExisting idAtCreate
  -> [Object                          otConf atConf dbTable otNative idAtExisting idAtCreate -> AnySVALUE]
  -> Presentation.Monad [Object       otConf atConf dbTable otNative idAtExisting idAtCreate]
  -> Presentation.Monad OL.ObjectList
objectList mbTitle atListSetup footerConstructor listOfMkObjectAction getObjects =
  OL.objectList mbTitle otSetup footerConstructor listOfMkObjectAction getObjects
  where
    otSetup = OS.mkObjectSetup atListSetup
