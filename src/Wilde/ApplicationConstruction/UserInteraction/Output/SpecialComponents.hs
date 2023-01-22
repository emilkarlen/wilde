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
      pure $ AnyCOMPONENT $ TableListComponent Nothing styledTable
  where
    mkSideContent mkButtons pk  = if null mkButtons
                                  then empty
                                  else LayoutValues.horizontal [mkBtn pk | mkBtn <- mkButtons]
