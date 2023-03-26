{-# LANGUAGE ScopedTypeVariables #-}

module Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectListComponent
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
import qualified Wilde.ObjectModel.Presentation as OmPres
import           Wilde.ObjectModel.Presentation.FooterRowsConstructor2
                   ( FooterConstructor, MkFooterConstructor )

import qualified Wilde.ApplicationConstruction.Presentation.ObjectListComponent as OLC
import qualified Wilde.ApplicationConstruction.Presentation.ObjectModel.ObjectSetup as OS


-------------------------------------------------------------------------------
-- - implementation -
-------------------------------------------------------------------------------


objectList
  :: forall otConf atConf dbTable otNative idAtExisting idAtCreate.
     OmPres.ATTRIBUTE_PRESENTATION atConf
  => WildeStyle
  -> Maybe WildeTitle
  -> AttributeTypeListSetup.Setup  otConf atConf dbTable otNative idAtExisting idAtCreate
  -> MkFooterConstructor           otConf atConf dbTable otNative idAtExisting idAtCreate
  -> [Presentation.Monad (Object   otConf atConf dbTable otNative idAtExisting idAtCreate -> AnySVALUE)]
  -> [Presentation.Monad (Object   otConf atConf dbTable otNative idAtExisting idAtCreate -> AnySVALUE)]
  -> Presentation.Monad [Object    otConf atConf dbTable otNative idAtExisting idAtCreate]
  -> Presentation.Monad AnyCOMPONENT
objectList
  tableStyle mbTitle atListSetup mkFooterConstructor
  getMkActionsLeft getMkActionsRight
  getObjects =
    OLC.objectList tableStyle mbTitle otSetup footerConstructor getMkActionsLeft getMkActionsRight getObjects
  where
    otSetup = OS.mkObjectSetup atListSetup

    footerConstructor :: FooterConstructor otConf atConf dbTable otNative idAtExisting idAtCreate
    footerConstructor = mkFooterConstructor colInfos
      where
        colInfos      :: [Maybe (Any (AttributeType atConf dbTable))]
        colInfos       = actionColIfNonEmpty getMkActionsLeft <>
                         atColInfos <>
                         actionColIfNonEmpty getMkActionsRight

        atColInfos     = map Just attributeTypes
        attributeTypes = AttributeTypeListSetup.getAts atListSetup

        actionColIfNonEmpty :: [a] -> [Maybe b]
        actionColIfNonEmpty xs | null xs   = []
                               | otherwise = [Nothing]
